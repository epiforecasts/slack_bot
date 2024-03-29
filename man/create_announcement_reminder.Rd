% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_announcement_reminder.R
\name{create_announcement_reminder}
\alias{create_announcement_reminder}
\title{Create announcement reminder for lab meeting}
\usage{
create_announcement_reminder(..., topic = "", advance = "ca. 15 minutes")
}
\arguments{
\item{...}{Arguments passed to \code{\link[=format_meeting_info]{format_meeting_info()}}}

\item{topic}{What will be the topic?}

\item{advance}{How long in advance the message is sent}
}
\description{
Create announcement reminder for lab meeting
}
\examples{
create_announcement_reminder()
create_announcement_reminder("Sebastian Funk")
create_announcement_reminder("Seb")
create_announcement_reminder("Sebastian Funk", "someone")
create_announcement_reminder(random = "Sebastian Funk")
create_announcement_reminder(chair = "Sebastian Funk", zoom_link = "https://example.com")

}
