# Check for plan package, install if not currently, and load from library.
if (!require(plan)){
  install.packages("plan")
  library("plan")
}

# Create a new gantt chart within a named object.
h <- new("gantt")

# Add tasks; the first can be used as a title.
h <- ganttAddTask(h,  # named gantt object
                  "Bluff Lake Drawdown")  # title of task (title for first line)
h <- ganttAddTask(h, "2019 Bathymetric Mapping", "2019-01-01", "2019-03-01", done = 100)
h <- ganttAddTask(h, "2019 Creel", "2019-03-01", "2019-10-31", done = 42)
h <- ganttAddTask(h, "2019 Spring Logger Deployment", "2019-05-01", "2019-05-07", done = 100)
h <- ganttAddTask(h, "2019 Drawdown", "2019-07-01", "2019-08-15",done=0)
h <- ganttAddTask(h, "2019 Fish Sampling", "2019-06-01", "2019-10-31", done = 0)
h <- ganttAddTask(h, "2019 Water Quality Sampling", "2019-06-01", "2019-10-31", done = 0)
h <- ganttAddTask(h, "2019 Fall Logger Deployment", "2019-11-01", "2019-11-07", done = 0)
h <- ganttAddTask(h, "2019 Refill", "2019-12-01", "2020-01-15", done = 0)
h <- ganttAddTask(h, "2020 Bathymetric Mapping", "2020-01-01", "2020-03-01", done = 0)
h <- ganttAddTask(h, "2020 Creel", "2020-03-01", "2020-10-31", done = 0)
h <- ganttAddTask(h, "2020 Spring Logger Deployment", "2020-05-01", "2020-05-07", done = 0)
h <- ganttAddTask(h, "2020 Drawdown", "2020-07-01", "2020-09-01", done = 0)
h <- ganttAddTask(h, "2020 Fish Sampling", "2020-06-01", "2020-10-31", done = 0)
h <- ganttAddTask(h, "2020 Water Quality Sampling", "2020-06-01", "2020-10-31", done = 0)
h <- ganttAddTask(h, "2020 Fall Logger Deployment", "2020-11-01", "2020-11-07", done = 0)
h <- ganttAddTask(h, "2020 Refill", "2020-12-01", "2021-01-15", done = 0)


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

Hooks1<-c(1,2,3)
Hooks2<-c(1,2,3)
sample(Hooks1, replace = F)
