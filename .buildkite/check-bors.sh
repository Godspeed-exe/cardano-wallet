#!/usr/bin/env bash

########################################################################
# This script is only meant to be run under Buildkite for the Bors
# staging branch.
#
# Example variables:
#
#   BUILDKITE_BRANCH=staging
#   BUILDKITE_PIPELINE_DEFAULT_BRANCH=master
#   BUILDKITE_REPO=https://github.com/cardano-foundation/cardano-wallet.git
#
########################################################################

set -euo pipefail

# Find the parents of the staging branch merge commit.
# This is a space-separated list of commit hashes.
staging_branch_parents=$(git show -s --pretty=%P HEAD)

# Find the commit hash of origin/master.
master_rev=$(git ls-remote $BUILDKITE_REPO $BUILDKITE_PIPELINE_DEFAULT_BRANCH | cut -f1)

if [[ "$staging_branch_parents" =~ "$master_rev" ]]; then
  exit 0
else
  echo "$BUILDKITE_PIPELINE_DEFAULT_BRANCH commit is $master_rev"
  echo "$BUILDKITE_BRANCH parent commits are $staging_branch_parents"
  echo
  echo "Refusing to merge because the pull request does not target $BUILDKITE_PIPELINE_DEFAULT_BRANCH."
  echo "You should only use Bors to merge into the $BUILDKITE_PIPELINE_DEFAULT_BRANCH branch."
  echo "Either change the PR base branch to $BUILDKITE_PIPELINE_DEFAULT_BRANCH, or merge manually."
  exit 1
fi
