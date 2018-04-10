# tootsville.org
Tootsville.org web content

This package contains the working copy of the Tootsville V game, as it is
being built / merged-into

## External Components

The game system is essentially made up of 8 pillars, or main components.
Three of these are completely external:

### The Wiki-wiki

The Wiki-wiki is a Mediawiki installation and is used as an external source
of documentation both within the game, and externally. The contents of the
Wiki-wiki are not maintained in this repository.

### Jumbo

The static game content, and larger or shared web content, are hosted on
Jumbo, and are not maintained in this repo, either.

### Tootsbook

Tootsbook is a Wordpress installation, and is likewise completely external.

## Internal Components

Within this repository are the five main, "internal" components.

### Login

The Login application manages user sign-up, character selection, and the
like from the front-end. It interacts mostly with the Users service.

### Play

The Play application is the "in-game" Javascript application that presents
the 3D world and Heads-Up Display (HUD) overlays.

### Users

The Users application is a REST server providing human and Toot user
information, and is used by both Login and Play.

### Gossip

The Gossip application is a directory service used to initiate the Gossipnet
connections between players (by Play).

### Indira

The Indira application quiesces and burgeons areas of the game world and
provides conflict resolution of user action lists.

## Operations

Deployment is spelled out in the bin/deploy-cluster script. In general, the
front-end Javascript applications are compiled down, compressed, and sent
off to their respective ("dumb," static) servers. 

The back-end application systems each run a respective copy of the monolithic Tootsville executable.
This is a single executable compiled from the same Common Lisp sources on
each host (to ensure CPU-OS-Compiler-Library match-ups and avoid surprises).

