Reproducible Research
Monday, April 30, 2012  14h-16h
Denis Haine (Université de Montréal)
Topics
Reproducible research was first coined by Pr. Jon Claerbout, professor of geophysics at Stanford University, to describe that the results from researches can be replicated by other scientists by making available data, procedures, materials and the computational environment on which these results were produced from.

This workshop intends to describe reproducible research, what it is and why you should care about it, and how to do it with the combination of R, LATEX, Sweave and makefile. Tips and tricks will also be provided.

Learning Objectives

    To get introduce to the concept of reproducible research,
    To get started with the implementation of reproducible research with R and Sweave,
    To produce a first Sweave document in LATEX.

Learning Objectives

Basic understanding of R and computing environment.
Packages

The workshop will try to give you a hands-on on producing Sweave files. For this, you will need R, LaTeX, Sweave and a text editor.


To use LaTeX on your computer, you have to install a TeX distribution. TeX distributions contains the typesetting system and collections of packages that allow you to write a document. The recommended distributions are TeX Live for Unix/Linux, MiKTeX for Windows, and MacTeX for Mac OS X. Note that you will need administrator rights on your computer. As a LaTeX distribution is a fairly large file (according to the OS, it can be over 1 Gb), it is better if you could download it on your computer before the workshop (i.e. have plenty of time and/or a high-speed internet connection). The facilitator will be present an hour before the start of the workshop to help if you encounter any problem to install LaTeX on your computer.


    Linux: a minimal LaTeX distribution should already be present on most Linux distributions. If you want a full installation on Ubuntu, you can also try sudo apt-get install texlive-full. You might also want the LaTeX plug-in for Gedit (sudo apt-get install gedit-latex-plugin).
    Windows: MiKTeX can be downloaded from http://miktex.org/2.9/setup.
    Mac: Go to http://www.tug.org/mactex/2011/ and download from that page the MacTeX.mpkg.zip package.


Sweave is included in all recent versions of R. We will also use the following packages: Hmisc and xtable.

Any text editor can be use for this workshop. However, proper text editors allowing syntax highlighting are better suited. A comprehensive list of text editors and their pros and cons can be found on this page for those interested.
