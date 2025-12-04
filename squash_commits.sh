#!/bin/bash
set -e

BRANCH="master"
BOT_AUTHOR_NAME="github-actions[bot]"
BOT_AUTHOR_EMAIL="41898282+github-actions[bot]@users.noreply.github.com"
CHUNK_SIZE=30
PRESERVE_LAST_N=5

# Create backup
git branch -f old_branch_backup "$BRANCH"

# Get all commits, newest first
ALL_COMMITS=($(git rev-list --reverse "$BRANCH"))

# Split into two: recent commits to preserve and older ones
NUM_COMMITS=${#ALL_COMMITS[@]}
let CUT_INDEX=NUM_COMMITS-PRESERVE_LAST_N
if [ $CUT_INDEX -lt 0 ]; then
  echo "Not enough commits to squash. Exiting."
  exit 0
fi
TO_SQUASH=("${ALL_COMMITS[@]:0:$CUT_INDEX}")
TO_KEEP=("${ALL_COMMITS[@]:$CUT_INDEX}")

# Start new branch from the first preserved commit's parent
FIRST_KEEP="${TO_KEEP[0]}"
BASE_COMMIT=$(git rev-parse "$FIRST_KEEP^")
git checkout -b cleaned "$BASE_COMMIT"

flush_bot_chunk() {
  local CHUNK=("$@")
  if [ "${#CHUNK[@]}" -eq 0 ]; then return; fi
  echo "Squashing ${#CHUNK[@]} bot commits..."
  if git cherry-pick --allow-empty --strategy-option=theirs --no-commit "${CHUNK[@]}"; then
    git commit --allow-empty --quiet -m "Squashed ${#CHUNK[@]} bot commits"
  else
    echo "Auto-resolving conflicts in bot chunk..."
    git add -A
    git commit  --allow-empty --quiet -m "Squashed ${#CHUNK[@]} bot commits (with forced resolution)"
  fi
}

has_single_child() {
  local commit="$1"
  # Count number of children: other commits that list this one as a parent
  local count
  count=$(git rev-list --all --children | awk -v c="$commit" '$2 == c || $3 == c {found++} END {print found+0}')
  [ "$count" -le 1 ]
}

cherry_pick_force() {
  local commit="$1"
  local msg=$(git log -1 --pretty=format:%s "$commit")
  echo "cherry picking: $msg"
  if git cherry-pick -m 1 --allow-empty --strategy-option=theirs "$commit"; then
    echo "Cherry-picked cleanly."
  else
    echo "Conflict in non-bot commit $commit; auto-resolving..."
    git add -A
    git commit  --allow-empty --quiet -m "$msg (forced resolution)"
  fi
}

# Process the commits to squash
BOT_CHUNK=()
for commit in "${TO_SQUASH[@]}"; do
  AUTHOR_NAME=$(git show -s --format='%an' "$commit")
  AUTHOR_EMAIL=$(git show -s --format='%ae' "$commit")
  if [[ "$AUTHOR_NAME" == "$BOT_AUTHOR_NAME" && "$AUTHOR_EMAIL" == "$BOT_AUTHOR_EMAIL" ]]  && has_single_child "$commit"; then
    BOT_CHUNK+=("$commit")
    if [ "${#BOT_CHUNK[@]}" -eq "$CHUNK_SIZE" ]; then
      flush_bot_chunk "${BOT_CHUNK[@]}"
      BOT_CHUNK=()
    fi
  else
    flush_bot_chunk "${BOT_CHUNK[@]}"
    BOT_CHUNK=()
    cherry_pick_force "$commit"
  fi
done

# Flush any remaining bot commits
flush_bot_chunk "${BOT_CHUNK[@]}"

# Now replay preserved commits
for commit in "${TO_KEEP[@]}"; do
  cherry_pick_force "$commit"
done

# Final check: compare heads
echo "Comparing 'cleaned' with original '$BRANCH'..."
if git diff --quiet cleaned origin/"$BRANCH"; then
  echo "✅ No differences between cleaned and origin/$BRANCH."
else
  echo "⚠️ Differences found between cleaned and origin/$BRANCH."
fi

echo "✅ Finished building cleaned branch. Review and run:"
echo "    git checkout $BRANCH"
echo "    git reset --hard cleaned"
echo "    git push origin $BRANCH --force   # <== Manual step"
