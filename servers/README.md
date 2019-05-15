# CIWTA

CIWTA distributed mesh game system

## History

This is  a game system based  upon the Romance game  server developed by
Bruce-Robert Pocock, with contributions  by Tim Heys, Ed Winkelman,
Gene Cronk, Robert Dawson, and others.  Romance 2 is a complete re-write
inspired by  the Java “original”  and written almost entirely  in Common
Lisp. However, some  portions are compiled to Javascript,  and there are
supporting components and tools in other languages.

This game system has proven in the past versions to be very flexible and
reliable;   however,    this   new   version   is    extremely   “alpha”
quality software.

Components are  being added in  chunks based  on various test  and pilot
programs that  have been created  individually, mostly in  the 2012-2017
time frame.

The pilot  program for CIWTA, from which  it takes its  name via
anagram, is Tootsville Ⅴ, a game accessible via http://Tootsville.org/ —
For         details         on          that         game,         visit
https://wiki-wiki.tootsville.org/wiki/wikiwiki/

Development is being financed entirely (as  of March, 2017) by the Fenn
Pocock family.

## Testing

Our         unit        tests         are        being         developed
for
[Travis-CI](https://travis-ci.org/romance-ii/ciwta/branches#),
[Circle CI](https://circleci.com/gh/romance-ii/ciwta/tree/tootsville),
and   (possibly?)  [Codeship](https://app.codeship.com/projects/181513).
Our     browser     integration     testing    is     being     designed
for [Selenium](http://seleniumhq.com/) with the generous gift of support
from [BrowserStack](http://browserstack.com/) for cross-platform testing
on Android, Apple, and MicroSoft platforms.

## Usage

At the  moment, we  are spending  more time  developing pieces  and less
time documenting. As  of the writing, very few  components have actually
been put  in place, and  even fewer unit  tests. Even the  JSCL compiler
version is undergoing somewhat heavy development.

If  you  have  a  serious  use-case for  this  program,  please  contact
Bruce-Robert Pocock directly.

In general, we do *not* expect this code to yet be interesting/useful to
others, but  we'd be  happy to  be proven wrong.  Many hands  make light
work, to many eyes all bugs are shallow, et al.

## License

This game  is free software; you  may play, distribute, and  alter it in
accordance  with the  terms of  the  GNU Affero  General Public  License
(AGPL); see LICENSE for details.

In partial waiver of § 13 of  that license: If your game provides a link
somewhere visible to a  user logging in to your game (eg,  on a login or
“play  now”  page)  with  an   appropriate  label  (eg,  “copyright”  or
“license”)  which in  turn gives  the AGPL-mandated  notification (“This
program is free software…”), that  is sufficient to meet your obligation
under this license.  Otherwise, you must include  the entire “prominent”
notice as  required by the  AGPL, § 13.  This exception only  applies to
a page which the user must reach to sign in (eg, not a splash or welcome
page which they could bypass by bookmarking the login page directly).

