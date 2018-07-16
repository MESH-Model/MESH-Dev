This contribution guide is only a draft.

CLASS-CTEM code uses an self-documenting tool called Doxygen. All new code should follow the doyxgen conventions and formatting.

# Doxygen
## Overview
Doxygen is a tool for producing documentation from commented code. For input it supports many code languages, 
including Fortran. Both fixed form code such as Fortran 77 (extension .f or .for) and free form code such as Fortran 90
(extension .f90) are recognized. There are many different supported outputs, including LaTeX and HTML.

## Quick Start (if you have made change to the code and doxygen comments)
   1. [Download Doxygen](http://www.stack.nl/~dimitri/doxygen/download.html) to your computer
   1. Run the command 'doxygen Doxyfile'.
   1. View the produced HTML by opening 'index.html' from within the newly created 'html' folder within your current directory.
   1. View the produced LaTeX pdf by using the created makefile:
      A. Run 'make' on the command line in the 'latex' folder in your current project's directory.
      A. Open the new pdf 'refman.pdf'

## Documenting The Code Through Comments Doxygen Recognizes
Doxygen will ignore regular code comments. Thus comments meant to be purely internal to the code can still be made. 
Commenting is slightly different depending on the version of Fortran being used.

### Free Form Fortran (.f90)
Start a comment the regular way with a '!' and then add either '<' or '>' to make it into a
comment block that Doxygen will recognize. To continue the comment on another line '!!' may also be used instead of '!>'.

### Fixed Form Fortran (.f, .for)
On a new line (not directly after other code) start a regular comment with the capital letter 'C',
then add either '<' or '>'. Comments can again be carried onto another line either using 'C!' or 'C>'.
It also appears to be fine for the older structured code to also use the newer style's type of comment.

### '<' versus '>'
'>' : This indicates to Doxygen that the comment refers to the code after the comment. 
'<' : This indicates to Doxygen that the comment refers to the code before the comment.

For example;

    top fortran code snippet
    !> comment 1
    middle fortran code snippet
    !< comment 2
    bottom fortran code snippet

will have both comments being related to the middle code snippet.

As copied from the 'getting started' documentation page:
"Place a special documentation block in front of the declaration or definition of the member, class or namespace.
For file, class and namespace members it is also allowed to place the documentation directly after the member."

In general, comment blocks must be before the code (and thus use '!>' and '!!' but not '!<'), 
with a few exceptions such as class and namespace members.

### Beyond Standard Comment Blocks

To make 'pretty' formatting, you can use [markdown](http://daringfireball.net/projects/markdown/syntax) and doyxgen will understand it (the switch for this is
turned on in our doxyfile).

Sometimes there is information that should be documented but is not necessarily related to any particular part
of the code. In this case structural commands or other specific commands can be put directly before the comment to tell Doxygen 
where to put the documentation. 

For example:

    !>\file
    !> documentation for this file

The '\file' command in the above code would tell Doxygen that the comment block is meant to be about the
file it is contained within, not the code above or below the comment.

NOTE: without a \file command comment within the document (or the file defined elsewhere), the file will not be listed. 

### LaTeX Formulas
For symbols and formulas (and LaTeX tables) to be rendered in the html and the pdf they must be escaped in
the comments. There are three options for escaping these symbols:
   1. For in-line mathematical symbols, surround the symbol with: '\f$'
   1. For unnumbered formulas on a separate line, surround the formulas with: '\f[' and '\f]'
   1. For LaTeX elements that are not in a math environment, surround the text with: '\f{LatexEnvironmentNameHere}' and '\f}'(https://www.stack.nl/~dimitri/doxygen/manual/formulas.html)[Examples of commented LaTeX formulas] (Link also in Further Reading)

### Including Images
   * Type into a comment block the '\image' command followed by a space and an environment (each environment such as html, 
     and latex will need a new command specifying the image is for them), then another space and quotes containing the file name
     , then optionally another space and quotes surrounding a caption for the image. An example of an image being included follows: 


        \image html "schematicDiagramOfClass.png" "Schematic Diagram Of CLASS"
        \image latex "schematicDiagramOfClass.png" "Schematic Diagram Of CLASS"


### References
To have citations within the text, Perl and Bibtex must be installed. To add a reference in the output text, the doxygen comment text write the doxygen command '\cite' followed by a space and a single word 
argument of the bibliographic identifier. For example, '\cite Arora2003838'. Then add the bibliographic information in 
bibtex format as in the bibliography file, biblio.bib, in the CLASS-CTEM repo.

### Formatting and Additional Tips & Tricks
   * Lists can be made in the comments by having a column aligned minus sign, plus sign, or asterisk to make a bullet point list,
     and a minus sign followed by '#' or a number followed by a dot will produce a numbered list.
   * Doxygen accepts HTML comment markers, so if you wish to ignore part of your comment block, you can surround the
     text with '<!-- hidden section here -->'. This can be particularly helpful if you want text in a certain place but do not want
     to break your one comment block.
   * '[ ]' followed by '( )' will be interpreted as a link in a doxygen command. If a link is desired, follow the form 

            [link text description](link)

     If the text is simply meant to be text and not a link, the simplest way to de-link the text, simply switch the order:'(text) [text]'.
   * If a comment block contains many spaces in front of the text it will be put in its own little box in the output instead 
     of simply flowing into the rest of the text. If the text is not meant to be separated, but the comment should be indented, 
     simply insert the spaces before the comment marker instead of after the comment marker. The first example below will generate
     a box, the second will not:

        !>             text
        !!             text
        !!             text
        !>text
        !!text
        !!text

   * If a module with multiple subroutines is being documented, they can be identified by grouping them. To do this, define a group at 
     the top of the file, for example '!>\defgroup disturbance_scheme_disturb', then add to that comment block any brief or detailed 
     description text you would like for the group, and then surround the subroutine that the group is to be used for with 

        !>\ingroup disturbance_scheme_disturb
        !!@{

     before the subroutine definition and '!>@}' after the 'end subroutine' code.
     
   * Adding an empty Doxygen comment line in the middle of a comment block will generate a new paragraph. If you want a new line but do not want a new paragraph,
     simply add a single '\n' (new line escape) at the end of the comment where you want a line break in the output.
   * To link to another documented file simply write the filename with the extension in a comment. Doxygen will look for this file in
     the project and, if it exists, automatically generate a link to the documentation page for the specified file.

### Further Reading
   * [Doxygen getting started guide](http://www.stack.nl/~dimitri/doxygen/manual/starting.html)
   * [putting LaTeX formulas in code comments](https://www.stack.nl/~dimitri/doxygen/manual/formulas.html)
   * [complete list of Doxygen comment commands](http://www.stack.nl/~dimitri/doxygen/manual/commands.html#cmd_intro)
   * [fortran comment blocks documentation](http://www.stack.nl/~dimitri/doxygen/manual/docblocks.html#fortranblocks)
   * [configuration options](http://www.stack.nl/~dimitri/doxygen/manual/config.html#cfg_extract_all) (pointing to 'extract all' explanation)
   * [Doxygen list formatting examples](https://www.stack.nl/~dimitri/doxygen/manual/lists.html)
   * [Doxygen's subset of HTML that it supports](https://www.stack.nl/~dimitri/doxygen/manual/htmlcmds.html) (HTML comments at the bottom of the page)
   * [Doxygen image command](https://www.stack.nl/~dimitri/doxygen/manual/commands.html#cmdimage)
   * [Doxygen links within comments](https://www.stack.nl/~dimitri/doxygen/manual/markdown.html#md_links)
   * [Doxygen automatic link generation](https://www.stack.nl/~dimitri/doxygen/manual/autolink.html)
