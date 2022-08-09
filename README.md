# Introduction

*Atlas* is a **document search engine** written in R programming language. The graphical interface is generated through the [**Shiny**](https://shiny.rstudio.com) package. Shiny applications are generally executed by a server and are made available to the user via web interface; *Atlas*, on the other hand, was designed to be run locally by the user.

*Atlas* was designed for Windows OS, some features require interfacing with the operating system, at the moment it has not been tested on Unix or Linux systems.
In the next chapters will be explained the purpose of *Atlas* and how to use it.

# The problem
 In the working reality of a big organization, it is very common to come into contact with a very large number of documents. These documents are written by different people who will use different thinking paradigms. With rare exceptions (it is very interesting to ask ourselves which ones) the corporate know-how is codified in an unstructured corpus of documents.
 Finding information manually is boring, time-consuming, and very ineffective. The 2020 pandemic and the consequent smart working have only made the situation worse.

# The solution
 The base solution is to use a search engine to search for keywords in document's corpus. A search engine consists of 3 parts:

-   a **crawler engine** that reads documents and creates an index;

-   a **storage unit**;

-   a **retrieval engine** that associates one or more documents with a user query;

 The trade-off is in the creation time, the size of the index and the time and effectiveness of associating a list of documents with a user query. A good search engine indexes only the most relevant information and organizes it for effective and efficient retrieval; it is complex and beautiful.

*Atlas* is trivial. It's the simplest search engine possible. It takes in input a file system path (a folder on your PC) and search for all the contained documents and creates an index organized by words.
That's it. No email addresses, no dates, no telephone numbers or numbers in general; any character other than the Latin alphabet (Cyrillic, greek, special.. ) is not seen by the engine.
The types of documents currently supported are:

-   doc;

-   docx;

-   pdf;

-   pptx;

-   rtf;

-   txt;

# How to use it
Atlas has a graphical interface so once started it should be easy to navigate. However it is developed in R, so you can't have a double-click installer. The installation and first start may not be too easy, let's see if I can help you.

## Are you an R programmer?

### A) No, I'm sorry
Well, if you are a human there is about a 99.9937% chance that you are not an R programmer, so I guess this is not something you need to worry about. You can try these steps:

1.  **Download R**, you can find
    [**here**](https://cran.mirror.garr.it/CRAN/) (the graphic in this page
    is not outdated, it's vintage).

2.  **Install R**;

3.  **Download Atlas code:** you can find
    [**here**](https://github.com/TommasoTarantola/atlas_search_engine).
    Click on "Code" green button and download ZIP;

4.  **Unzip**;

5.  **Launch atlas_bootstrap.Rscript:** Ok, now it gets weird. You have
    to associate the .Rscript extension with the Rscript.exe executable
    so that in the future you just need to double-click on
    atlas_bootstrap.Rscript file to launch *Atlas*. It is quite easy:

    - Right-click on atlas_bootstrap.Rscript and click on "open
        with" \> Windows should ask you what program you want to use;

    -   Click on "chose another app" \> Windows should open a new window
        where you can choose the app .exe file;

    -   Now you need to find the Rscript.exe file in R installation
        folder. It should be under C:/ Program Files / R / R-xxx / bin /
        Rscript.exe.

    -   Click on it and should be done

6.  At first start *Atlas* will automatically download all required
    external libraries, it may take a while, and by that I mean it takes
    a really long time, trusts me, have a coffee. Once the process is
    complete it will launch the app.

7.  If all steps above are successful and the PC hasn't exploded, the
    installation should be over.

### B) Yes, I am
Perfect, I'm sorry for you but I'm glad you made the same bad choices of mine.
The *Atlas* code is organized in 3 main files:

-   **atlas_boostrap.R:** as you can imagine it is a bootstrap, it is
    used to download and load external libraries and launch the app on
    the browser. It is necessary mainly for non-R user, but if you want
    you can run it from RStudio and it should work;

-   **atlas_functions:** it is a collection of functions, mostly for
    managing the logic of creating and updating the index;

-   **app.R:** it is the Shiny main app, manages the GUI and, on the
    server side, the retrieval engine logic.

### C) Wait, what it is R?
A bad thing. You can refer to point A). More about R on [**R wiki**](https://en.wikipedia.org/wiki/R_(programming_language)),
[**R**](<https://www.r-project.org>) and
[**RStudio**](https://www.rstudio.com) pages.

## The app
![atlas_main](https://user-images.githubusercontent.com/60482239/116578202-7789f480-a911-11eb-9bb4-f7981d226e0b.png)

### The index
Now that you have successfully installed *Atlas* you can start using it.
First, you need to identify in which folder are placed the documents you are interested in indexing. Copy the folder path and insert it in the "Document folder path" box, then press "Create index", and wait until the message "Index created" appears. That's it.
If at any time you believe that a document has been inserted, modified or deleted press "Update index" to read or re-read the new documents.
All information on indexed documents is contained in the "index" folder.

### The search engine
To search, enter the words of interest in the box at the top left and press the magnifying glass.
There are 3 search modes:

-   **Normal:** finds each document where all the searched words in the user
    query appear;

-   **Fuzzy:** finds each document where all the searched words in user query
    appear. It has two parameters that allow you to moderate this
    behavior:

    -   **Max number of not matched word:** so that a document does not
        have to contain exactly all the words in the user query;
    -   **Max number of not matched characther in a word:** so that
        words similar, but not identical, to those in the user query are
        still counted. Similarity is calculated as
        [**Damerau-Levenshtein**](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance)
        distance from the user query word.

-   **Synonyms:** find each document where at least one of the synonyms
    of the words in the user query appear.

The first 100 documents sorted by similarity to the user query will be returned. No characters other than the Latin alphabet will be considered.
