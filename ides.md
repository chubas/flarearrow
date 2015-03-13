# OCaml IDEs and Editors #

So far, we've used Gedit, Kate, Vim and Eclipse.

- Gedit and Kate mostly for scripts and tests. (integrating a console, syntax highlighting)
- Vim and Eclipse have their respective plugins for OCaml

Here is a description of three Eclipse plugins for Eclipse: http://www.cocan.org/ocaml_and_eclipse

Plugins Mentioned:
  1. OCaml Development Tools. http://ocamldt.free.fr/
  1. EclipseFP (Eclipse OCaml Support) http://eclipsefp.sourceforge.net/ocaml/
  1. OcaIDE http://ocaml.eclipse.ortsa.com:8480/ocaide/


Eclipse is a great alternative for integrated projects. As far as we've tested, OcalIDE is the best option. Providing autocompletion, syntax hightlighting and support for  OCamlbuild and Omake, which are useful for makefiles and general project integration.


---


## Installing OcaIDE ##

Open Eclipse, click on Help > Software Updates > Find and Install, in the page that opens, click on "Search for new features to install" and then on Next, then, click on "New Remote Site" and enter OcaIDE as a name, and http://ocaml.eclipse.ortsa.com/ as URL. Select "OcaIDE" in the list, and click Finish.


OcaIDE has some issues with gcj (either launch Eclipse with gij or sun virtual machines, or configure your default java vm).

We suggest using Eclipse version 3.3 or greater. For more info about installing OcaIDE in Eclipse 3.2 go to http://ocaml.eclipse.ortsa.com:8480/ocaide/eclipse32.html


### Using OcaIDE ###

A great video tutorial [here](http://ocaml.eclipse.ortsa.com:8480/ocaide/tutorials/3-projects/projects.htm)