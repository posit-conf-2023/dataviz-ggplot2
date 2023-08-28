Engaging and Beautiful Data Visualizations with ggplot2
================

### posit::conf 2023

by Cédric Scherer

-----

:spiral_calendar: &ensp;September 18, 2023  
:alarm_clock:     &ensp;09:00 - 17:00  
:hotel:           &ensp;ROOM TBD  
:writing_hand:    &ensp;[pos.it/conf](http://pos.it/conf)

-----

## Overview

### Description

Creating effective and easily accessible data visualizations of high quality in an efficient and preferably reproducible way is an essential skill for everyone working in a data-related field. Luckily, by leveraging the functionality of ggplot2, the most famous package for data visualization with R, and related extension packages one can create highly customized data visualization without the need for post-processing.

This workshop provides everything one needs to know to create and customize numerous chart types with ggplot2. Participants will learn the most important steps and helpful tips to create visually appealing and informative graphics with a code-only approach. The power of ggplot2 and related extension packages will be illustrated with advanced real–life examples that help to understand useful coding tricks and the process of creating engaging and effective visualizations. The workshop will particularly focus on more advanced tasks with ggplot2 such as styling labels and titles, customizing themes and visual aesthetics, and using less-common chart types.

### Audience

This course is for you if you:

* already know how to create basic graphics with the ggplot2 package,
* aim to improve the design of your ggplot outputs, and
* want to learn how to create more complex charts that feature multiple layers, annotations, text styling, custom themes, and more.


## Schedule

| Time          | Activity                 |
| :------------ | :----------------------- |
| 09:00 - 10:30 | Fundamentals & Workflows |
| 10:30 - 11:00 | *Coffee break*           |
| 11:00 - 12:30 | Working with Text        |
| 12:30 - 13:30 | *Lunch break*            |
| 13:30 - 15:00 | Working with Themes      |
| 15:00 - 15:30 | *Coffee break*           |
| 15:30 - 17:00 | Working with Colors      |


## Prework

As this workshop focuses on (more) advanced topics, a solid understanding of the fundamentals of ggplot2 and also data preparation in R is helpful. The concepts of how to initialize a ggplot, map variables to aesthetics, and how aesthetics relate to scales should be familiar to you. If you want to refresh your knowledge, I suggest going through the first two chapters of the ggplot2 book by Hadley Wickham et al. ([introduction](ggplot2-book.org/introduction) and [getting started](ggplot2-book.org/getting-started)). The “R for Data Science” book also offers a good [overview of ggplot2](r4ds.hadley.nz/data-visualize) as well as [on data wrangling with the tidyverse](r4ds.hadley.nz/data-transform).

You will need to bring your own laptop to the workshop with a recent version of R (4.3.1 or newer) and RStudio (2023.06.1 or newer) as well as a bunch of R packages and a few typefaces installed.

### Install R and Rstudio: 

Download the most recent version of R (cloud.r-project.org, at least version 4.2) and Rstudio (rstudio.com/products/rstudio/download, at least version 2023.06.1) and follow the installation steps.

### Install R Libraries

You can install all required R packages at once by running the following code in the R command line:

```
pkgs <- c("ggplot2", "dplyr", "tibble", "tidyr", "readr", "forcats", "stringr", "lubridate", "here", "scales", "ragg", "systemfonts", "racartocolor", "scico", "prismatic", "patchwork", "ggtext", "ggforce", "ggrepel")
unavailable <- setdiff(pkgs, rownames(installed.packages()))
install.packages(unavailable)
```

To run the code, open up Rstudio. Copy–paste the code in the console pane (by default in the lower left) and hit enter. Several messages should pop up. Scan these messages that are returned for errors and troubleshoot them if necessary. Warnings and other informational messages can be ignored.

A few packages are not strictly needed but are used in some of the course material. If you wish to install all packages, run the following code as well:

```
pkgs_opt <- c("viridis", "RColorBrewer", "MetBrewer", "ggthemes", "ggsci", "camcorder", "colorspace", "remotes")
unavailable <- setdiff(pkgs_opt, rownames(installed.packages()))
install.packages(unavailable)
remotes::install_github("clauswilke/colorblindr")
```

If, for some reason, you cannot install these packages or the latest versions of R and RStudio, we will have a Posit Cloud workspace set up that can be used instead. But you will still need to bring your laptop.

### Install Typefaces

We will make use of custom fonts during the course. If you want to follow all contents locally, make sure the following typefaces are installed:

* Spline Sans: (fonts.google.com/specimen/Spline+Sans)[https://fonts.google.com/specimen/Spline+Sans]
* Spline Sans Mono: (fonts.google.com/specimen/Spline+Sans+Mono)[https://fonts.google.com/specimen/Spline+Sans+Mono]
* Asap: (fonts.google.com/specimen/Asap)[https://fonts.google.com/specimen/Asap]
* Hepta Slab: (fonts.google.com/specimen/Hepta+Slab)[https://fonts.google.com/specimen/Hepta+Slab]

You can download all typefaces as a single zip file here.

### For Mac OS Users

If you want to save your visualization to PDF, please make sure that XQuartz (xquartz.org) is installed which is needed to use the cairo pdf device. 

### Course Communication

We will be using Discord as our main communication method during the workshop. To make the process go smoothly:
* Please sign up for an account at [discord.com](https://discord.com) if you don’t already have one.
* Make sure your [display name](https://support.discord.com/hc/en-us/articles/12620128861463-New-Usernames-Display-Names#h_01GXPQABMYGEHGPRJJXJMPHF5C) is the one you used to register for the conference.
In your “About Me”, put the name of your workshop(s).

Closer to the start of the conference, you will be invited to join the posit::conf Discord server. Once you’ve accepted the invite, we will add you to the channel(s) for the workshop. 


## Instructor

[Dr Cédric Scherer](https://cedricscherer.com) is a data visualization designer, consultant, and instructor helping clients and workshop participants to create engaging and effective graphics. As a graduated ecologist, he has acquired extensive hypothesis–driven research experience and problem–solving expertise in data wrangling, statistical analysis, and model development. As an independent data visualization designer, Cédric later combined his expertise in analyzing large data sets with his passion for design, colors, and typefaces. Cédric has designed graphics across all disciplines, purposes, and styles applying a code–first approach, and regularly talks about data visualization design and ggplot2 techniques. Due to regular participation in social data challenges such as #TidyTuesday, he is now well known for complex and visually appealing figures, entirely made with ggplot2, that look as if they have been created with a vector design tool.

-----

![](https://i.creativecommons.org/l/by/4.0/88x31.png) This work is
licensed under a [Creative Commons Attribution 4.0 International
License](https://creativecommons.org/licenses/by/4.0/).
