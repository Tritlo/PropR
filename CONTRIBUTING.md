Contributing
============

Commit conventions
-------------------

Commits should be atomic and cleanly implement an improvement or feature, and a test-case thereof.
We use the following tags:

+ [Feature] - A `MAJOR` improvement that allows us to fix new classes of programs or considerably improves the speed or quality of repairs
+ [Improvement] - Improvements to repair speed or quality, but should not affect calling conventions
+ [Bugfix] - Small fixes 
+ [Refactor] - Changes to types or algorithms that does not impact functionality, but may still be a `MAJOR` change in that call conventions might changes.
+ [Documentation] - Changes to configuration or comments only
+ [WIP] - For work in progress commits only, should not appear on the main branch.

Any commits except very minor fixes should contain a paragraph or two describing the fix in more details below the short commit message.

Commits which change functionality or add new classes of repairs should include a test which covers that class.

Branch conventions
------------------

Branches are used for intermediate work in progress, such as fixes, improvements or features.

Commits should be marked with the `[WIP]` tag. Branches must be merged to master in one clean commit rebased on top of master
and squished before merging (use the GitHub `Squash and merge` feature or do it manually before merging). The merge commits
should have a short message and appropriate tag, with a longer description below describing the commit and its motivations.

Note that pull requests without proper documentation or tests will not be accepted.
