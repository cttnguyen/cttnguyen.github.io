---
author: Crystal Nguyen
date: "2019-10-15"
description: Welcome to our website, and how to host a website written in RMarkdown through GitHub Pages
image: images/blog/engagement_filtered.JPG
image_webp: 
title: First Post!
---

Hello, friends and family! 
  
  I am dedicating this blog to all things DIY that Daniel and I work on throughout the wedding planning process. This will include, but may not be limited to, I Do Crew proposals, invitations, gifts, and even the very website you are reading from now. I don't expect all or even any of you to find this remotely interesting, but maybe you will! At the very least, I wanted to document the neat things we come up with and describe how to do them in case other crafty couples wanted to learn from our successes and our mistakes.

  If you didn't already know, I really love crafting. The process of taking raw materials and creating something appreciable brings me great joy whether it's baking, wood crafting, or scrapbooking. Until recently, I have always been a busy student without a lot of time available for crafting. However, I now work that wonderful 9-5 and, better yet, have an occassion to take on some really great crafting projects: our engagement! 

  When we first sat down to discuss the vision of our wedding and what experience we wanted to walk away with, we agreed that we wanted our personality as a couple to really shine through the decisions. Rather than purchasing all of our wedding "stuff," we figured we could save a few bucks by doing some of it ourselves. Moreover, we could better tailor our products when building our wedding brand to be the same across everything we distribute, starting with our wedding website.

  We are, like many other engaged couples, using sites like WeddingWire and theknot in the planning process, but we just weren't satisfied with the options for creating a wedding website. We couldn't get the exact colors we wanted on all the website elements, there weren't a lot of options for fonts, and, most of all, <b>the data format of the guest list and RSVP form were not conducive to analysis</b>. This last point probably sounds really silly, but it was a deal breaker for us. 

  As a data scientist and research analyst by trade, Daniel and I could not stand the thought of willingly creating a guest list that was not easy to manipulate in data analytic software. With my background in creating flexdashboards in {{< tablink "R" "https://r-project.org/" >}} and Daniel's experience with collaborative version control in {{< tablink "GitHub" "https://github.com/" >}}, we knew there had to be a way to get this done. Now, this is most certainly <strong>not</strong> the most intuitive way to create a website. It definitely isn't the easiest (trust me, we did a pretty thorough search for web hosting options through R). Most web developers would use HTML programming, not statistical programming, but it's what we know! 

  So, without further ado, how the heck does one create a website using a language not conventional for building a website (rivetting, I know)? First off, I would describe R as a great tool for anyone looking to be a data scientist or analyst. The software is free to download and completely open source, making it an ideal sandbox for data junkies like Daniel and me. More recently, R's capabilities in making dashboards for data visualization has become quite popular (see {{< tablink "here" "https://rmarkdown.rstudio.com/flexdashboard/examples.html" >}} for examples). Some of these kinds of dashboards (flexdashboards) can produce html output, a file format you can easily save and send to others, and it will open and function conveniently for them.

  But we want a website: a URL that guests can search for in their browser, not a file we have to email to all of our guests individually! {{< tablink "GitHub Pages" "https://pages.github.com/" >}} to the rescue. GitHub is a place where you can practice version control -- it's like the track changes feature on a Google document but for any file that you may be making frequent edits to. GitHub Pages lets you upload the html file you may have, and it will host that file as a functioning website with a domain name of your choosing. Other sites and apps like Google Drive actually let you do this as well, but GitHub is a great, free option when multiple people are working together on the same project.

  So we got to work. Daniel picked up some flexdashboard experience, and I got the hang of GitHub. We both had to learn a small amount of HTML programming, but we got this project up and running within a matter of hours. That's pretty quick as far as collaborative deliverables go! 
  
  You may have noticed that our website is password protected. It was important to us to maintain some level of privacy. The free version of GitHub is intended for open access, encouraging collaboration and easy distribution of content and ideas. You can follow {{< tablink "these instructions" "https://github.com/matteobrusa/Password-protection-for-static-pages/" >}} to implement password protection on <strong>any</strong> website. It was super easy. You basically just have to know how to create and rename a folder, and you're set.

  We still have a ways to go. Not all of our tabs are set up. We'll need to link our RSVP page to a Google form to collect RSVPs. Who knows if the site will be horribly slow once we add photos to the gallery. Maybe I'll post an update later as to how it goes. Thus far, it's been just a really fun, nerdy bonding activity for us.

  Again, this was definitely not the most convenient way to create and host a wedding website, but this was absolutely the only way for Daniel and I to go about creating and hosting ours. Not only did it allow for complete customization of the end product, but building our website from the ground up in our preferred programming language has already been the ultimate kick off to our engagement. We both love data and programming in R; why not incorporate that into our wedding? My favorite part of building our website this way is that it is just totally novel. Flexdashboards are typically purposed for displaying visuals for data, not as a media for wedding information, but this challenge was merely an exciting puzzle for two nerds to solve. 

That's all for now. Stay tuned for how-tos, details, tips, and inspiration for DIY wedding projects.

<br>
<br>
<br>
<br>