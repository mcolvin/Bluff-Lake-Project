# Check for plan package, install if not currently, and load from library.
if (!require(plan)){
  install.packages("plan")
  library("plan")
}

# Create a new gantt chart within a named object.
h <- new("gantt")

# Add tasks; the first can be used as a title.
h <- ganttAddTask(h,  # named gantt object
                  "Bluff Lake 2020")  # title of task (title for first line)
h <- ganttAddTask(h, "2019 Bathymetric Mapping", "2019-01-01", "2019-03-01", done = 100)
h <- ganttAddTask(h, "2019 Creel", "2019-03-01", "2019-10-31", done = 100)
h <- ganttAddTask(h, "2019 Logger Deployment", "2019-05-01", "2019-05-07", done = 100)
h <- ganttAddTask(h, "2019 Logger Deployment", "2019-7-01", "2019-7-07", done = 100)
h <- ganttAddTask(h, "2019 Logger Deployment", "2019-9-01", "2019-9-07", done = 100)
h <- ganttAddTask(h, "2019 Logger Deployment", "2019-11-01", "2019-11-07", done = 0)
h <- ganttAddTask(h, "2020 Logger Deployment", "2020-01-07", "2020-01-14", done = 0)
h <- ganttAddTask(h, "2020 Logger Deployment", "2020-03-01", "2020-03-07", done = 0)
h <- ganttAddTask(h, "2020 Logger Deployment", "2020-05-01", "2020-05-07", done = 0)
h <- ganttAddTask(h, "2020 Logger Deployment", "2020-07-01", "2020-07-07", done = 0)
h <- ganttAddTask(h, "2020 Logger Deployment", "2020-09-01", "2020-09-07", done = 0)
h <- ganttAddTask(h, "2020 Bathymetric Mapping", "2020-01-10", "2020-03-01", done = 0)
h <- ganttAddTask(h, "2020 Creel", "2020-03-01", "2020-10-31", done = 0)
h <- ganttAddTask(h, "2019 Fish Sampling", "2019-11-01", "2020-03-30", done = 0)
h <- ganttAddTask(h, "2020 Water Quality Sampling", "2019-11-01", "2020-10-31", done = 0)


# Use tiff function to write plot to file.
# tiff(filename = "ganttProject.tiff",
#      width = 750, height = 500, pointsize = 12)

par(lend = "square")
    # default is round
    #mar = c(5, 4, 5, 1),
    #oma = c(1, 1, 2, 1))
plot(
  h,
  ylabel = list(font = ifelse(is.na(h[["start"]]), 2, 1)),  # task labels on y-axis
  event.time = Sys.Date(),  # vertical line for current day
  event.label = "Today"  # label for current day line
)
legend(
  "topright",  # where to place legend
  legend = c("Completed", "Not complete"),  # legend labels
  pch = 22,  # shape
  pt.cex = 2,  # point size
  pt.bg = gray(c(0.3, 0.9)),  # point background color
  bty = "n",  # include legend box
  # xpd = TRUE, # optional; legend can go outside chart
  inset = c(0, 0)  # legend position adjustment
)

# dev.off()  # to close graphic device and write to 


# Some references and examples.
# https://cran.r-project.org/web/packages/plan/plan.pdf
# https://cran.r-project.org/web/packages/plan/vignettes/plan.html
# http://dankelley.github.io/plan/examples.html
