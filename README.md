# cttnguyen.github.io

To make a new blog post:

Add a new .md file in `content/english/blog`. See existing blog posts for YAML.

Place any new images in `static/images/blog`. When referencing the image path in the YAML use `images/blog/<filename>`. When referencing the image path in the body use `../../images/blog/<filename>`. Ensure that the filename is case sensitive! (`.jpg` vs `.JPG`)

When ready to push, use command `blogdown::build_site()` to knit. Then simply add, commit, and push as usual to `master` (or to `dev` followed by a merge into `master`). This will trigger the action set up, where `gh-pages` will automatically update the page hosting the site. After a minute or so, check `www.TheLuckettest.com` for changes.
