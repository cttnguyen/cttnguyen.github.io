git checkout master
git pull

Rscript make_rsvp_plot.R
Rscript -e "rmarkdown::render('index.Rmd')"

git add rsvp.rds
git add index.html

git commit -m "Update RSVP map"
git push origin master

