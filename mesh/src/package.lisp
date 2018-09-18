(defpackage :aelius-galenus
  (:nicknames #:galen)
  (:use :cl :jscl/ffi :romance)
  (:documentation "a  system for quiescing  and burgeoning areas  of the
 game world;  that is,  it stops  actively performing  simulations upon
 areas that no-one can see, but then “spins up,” or burgeons, that area
 before a  character enters it, performing  some “fast-forward” actions
 to  bring  it  up  to  date.  This  works  with  Vitruvius,  Rabirius,
 and Frontinus."))
(defpackage :appius-claudius-caecus
  (:nicknames #:appius)
  (:use :cl :jscl/ffi :romance)
  (:documentation  "Appius manages  the  mesh communications  at a  high
 level, ensuring  best-case routing  and avoiding  potential netsplits.
 The  implementation is  independent of  the underlying  packet network
 encoding (eg, UDP, TCP, WebSockets, or WebRTC)"))
(defpackage :assets
  (:use :cl :jscl/ffi)
  (:export #:fetch-bytes)
  (:documentation "The  asset manager subsystems  fetch data from  the Net
 for the local client."))
(defpackage :assets.archives
  (:use :cl :jscl/ffi)
  (:export #:catalog<-archive #:asset<-archive)
  (:documentation  "Assets can  be bundled  into archives;  this package
 provides services to catalog and extract them."))
(defpackage :assets.service
  (:use :cl :jscl/ffi)
  (:documentation "The asset management process lives in a separate process/worker thread."))
(defpackage :assets.torrent
  (:use :cl :jscl/ffi)
  (:documentation "Assets are usually distributed via WebTorrent. This is the interface."))
(defpackage :babylon
  (:use :cl :jscl/ffi)
  (:documentation "Babylon.js is our 3D engine; this is the Lisp wrapper."))
(defpackage :chatter
  (:use :cl :jscl/ffi)
  (:export #:place-call
           #:answer-call
           #:send-sms
           #:send-mms-photo
           #:send-mms-audio
           #:send-call-to-voice-mail
           #:present-sms
           #:present-mms-photo
           #:present-mms-audio
           #:present-voice-mail
           #:present-mobile-conversation
           #:present-mobile-conversations)
  (:documentation "This package contains the in-game mobile phone services."))
(defpackage :clodia-metelli-pulcher
  (:nicknames #:clodia)
  (:use :cl :jscl/ffi :romance)
  (:documentation "Clodia  Metelli Pulcher,  the server for  \"real\" AI
 characters.   \"Real  AI\"   meaning  that   these  are   artificially
 intelligent logical agents, capable  of establishing and seeking goals
 for  themselves, operating  upon the  game world  in the  same way  as
 a player-character might; these are  not \"scripted AI's\" of the sort
 which carry out a fixed task repeatedly."))
(defpackage :device.ambient-light
  (:use :cl :jscl/ffi)
  (:documentation "Wrappers that interact with ambient light sensors."))
(defpackage :device.network
  (:use :cl :jscl/ffi)
  (:export #:onlinep
           #:metered-network-p)
  (:documentation "Wrappers that interact with network status detection."))
(defpackage :device.orientation
  (:use :cl :jscl/ffi)
  (:export #:portraitp
           #:landscapep
           #:get-precise-orientation
           #:compass-heading)
  (:documentation "Sensing the device's orientation and accelerometers"))
(defpackage :device.vibration
  (:use :cl :jscl/ffi)
  (:export #:vibrate)
  (:documentation "Haptic feedback"))
(defpackage :gaius-asinius-pollio
  (:nicknames #:asinius)
  (:use :cl :jscl/ffi :romance)
  (:documentation   "Gaius   Asinius   Pollio,  the   database   module.
 While  Romance  II does  not  \"constantly  ride the  database,\"  for
 purposes of  failure-proofing and  conserving RAM,  it does  rely upon
 a Postgres database back-end for  its persistent storage of game-world
 data, which  can also be  used by  reporting tools to  generate ad-hoc
 queries about the game world."))
(defpackage :gaius-julius-caesar
  (:nicknames #:caesar)
  (:use :cl :jscl/ffi :romance)
  (:documentation  "the module  which  monitors and  controls all  other
 components. Caesar is  being extended to have the ability  to bring up
 and  down  other services,  as  well  as provide  \"health\"  monitoring
 of them."))
(defpackage :gaius-lutatius-catulus
  (:nicknames #:lutatius)
  (:use :cl :jscl/ffi :romance)
  (:documentation "which translates between  plain written text forms of
 language  and   the  internal  \"propositions\"  used   by  the  game.
 It enables the game server (and particularly, AI characters) to create
 English   (and  someday,   other  languages)   sentences  from   these
 \"propositions,\"     and    parses     human    text     back    into
 \"propositional\" form."))
(defpackage :gossipnet.events
  (:use :cl :jscl/ffi)
  (:documentation "The event routing subsystem"))
(defpackage :gossipnet.genesis
  (:use :cl :jscl/ffi)
  (:documentation "Initial set-up (or reconnection) through the Whitney server"))
(defpackage :gossipnet.integrity
  (:use :cl :jscl/ffi)
  (:documentation "Maintaining integrity of the Gossipnet"))
(defpackage :gossipnet.net
  (:use :cl :jscl/ffi)
  (:export #:start
           #:stop)
  (:documentation "Low-level networking details of the Gossipnet"))
(defpackage :login
  (:use :cl :jscl/ffi)
  (:export #:google-login
           #:facebook-login)
  (:documentation "OAuth-style logins"))
(defpackage :lucius-aemilius-regillus
  (:nicknames #:regillus)
  (:use :cl :jscl/ffi :romance)
  (:documentation " Lucius Aemilius Regillus, which performs pathfinding
 through the  game's 3D  environment. Regillus  understands \"difficult
 terrain\"  and  can  provide  pathfinding  using  \"acrobatics\"  when
 necessary (e.g. jumping)"))
(defpackage :marcus-vitruvius-pollio
  (:nicknames #:vitruvius)
  (:use :cl :jscl/ffi :romance)
  (:documentation "Marcus Vitruvius Pollio, which manages the biological
 simulation  for plants  and  animals.  Vitruvius handles  \"biological
 stats\" like health,  strength, and stamina, as well  as morphology of
 the animal/plant's body, and can  use a simple genetics/heredity model
 to produce new life-forms based upon their parentage."))
(defpackage :narcissus
  (:use :cl :jscl/ffi :romance)
  (:documentation "Physics"))
(defpackage :parrot
  (:use :cl :jscl/ffi)
  (:export #:squawk
           #:peep
           #:feather)
  (:documentation "Notifications from the game client itself"))
(defpackage :parrot.buddy-list
  (:use :cl :jscl/ffi)
  (:export #:add-buddy
           #:remove-buddy
           #:present-buddy
           #:present-buddy-list)
  (:documentation "The  buddy list system.  Actually might move  off the
 Parrot interface into the mobile phone."))
(defpackage :parrot.child-account
  (:use :cl :jscl/ffi)
  (:export #:add-child
           #:present-child
           #:present-children
           #:child-play-log)
  (:documentation "Setting up a child account"))
(defpackage :parrot.child-login
  (:use :cl :jscl/ffi)
  (:export #:request-child-login
           #:parent-validate-child-login
           #:parent-set-child-quota)
  (:documentation "Child requests login, and parent approves it."))
(defpackage :parrot.new-player
  (:use :cl :jscl/ffi)
  (:export #:new-player)
  (:documentation "Creating a new Toot character"))
(defpackage :player-input.bluetooth-gamepad
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Integration with Bluetooth gamepad devices"))
(defpackage :player-input.bluetooth-wiimote
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Integration with the Wii Remote over Bluetooth"))
(defpackage :player-input.events
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Abstract player input system"))
(defpackage :player-input.gamepad
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Gamepad control"))
(defpackage :player-input.keyboard
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Keyboard controls"))
(defpackage :player-input.lipread
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Reading player's facial shape to control characters' faces"))
(defpackage :player-input.listen
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Speech transcription/recognition interface."))
(defpackage :player-input.tabletpad
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation  "Using  a tablet/phone  as  a  gamepad; this  is  the
 receiver from the remote app."))
(defpackage :player-input.touch
  (:use :cl :jscl/ffi)
  (:export #:configure
           #:start
           #:stop)
  (:documentation "Touchscreen interface"))
(defpackage :publius-cornelius-tacitus
  (:nicknames #:tacitus)
  (:use :cl :jscl/ffi :romance))
(defpackage :rabirius
  (:use :cl :jscl/ffi :romance)
  (:documentation "Rabirius  is the module responsible  for handling the
 geometry of the game world's  \"map,\" i.e. inanimate objects, such as
 the   ground,  rocks,   and  (in   concert  with   Vitruvius)  plants.
 Rabirius  can also  automatically  generate terrain  areas based  upon
 vague details,  allowing a game designer  to \"sketch\" a loose  map and
 have the  game system fill  in the  finer points. This  works together
 with Frontinus."))
(defpackage :rahab
  (:nicknames #:spy)
  (:use :cl :jscl/ffi :romance)
  (:documentation "The “spy” interface for Operators"))
(defpackage :sextus-julius-frontinus
  (:nicknames #:frontinus)
  (:use :cl :jscl/ffi :romance)
  (:documentation "Frontinus  handles water  cycles and weather  for the
 game  world,  and  (somewhat  tangentially)  heavenly  bodies  of  the
 sky (i.e.  phases of the  moon, apparent  motion of the  sun, moon(s),
 stars)  as well.  Frontinus  applies a  basic model  to  the flow  and
 current of rivers and streams, waves  in lakes and seas, and cloud and
 precipitation patterns, as well as wind forces."))
(defpackage :ux.events
  (:use :cl :jscl/ffi)
  (:documentation "General user experience events system"))
(defpackage :ux.gossip-mouse
  (:use :cl :jscl/ffi)
  (:export #:present-gossip-mouse)
  (:documentation "Gossip Mouse agents for the Gossipnet"))
(defpackage :ux.make-noise
  (:use :cl :jscl/ffi)
  (:export #:toot)
  (:documentation "Speech synthesis for elephant sounds particularly"))
(defpackage :ux.overlay
  (:use :cl :jscl/ffi)
  (:documentation "Overlays that appear over the 3D scene"))
(defpackage :ux.parrot
  (:use :cl :jscl/ffi)
  (:export #:present-parrot)
  (:documentation "The Mist Parrots interface"))
(defpackage :ux.speak
  (:use :cl :jscl/ffi)
  (:export #:speak)
  (:documentation "Speech synthesis"))
(defpackage :ciwta
  (:use :cl :jscl/ffi)
  (:nicknames #:romance)
  (:export #:-then->)
  (:documentation "Common utility functions"))
(defpackage :webdebug
  (:use :cl :jscl/ffi)
  (:export #:debugger
           #:debugger-js)
  (:documentation "Debugger"))
(defpackage :webinspect
  (:use :cl :jscl/ffi)
  (:export #:inspect)
  (:documentation "Inspector"))
(defpackage :webrepl
  (:use :cl :jscl/ffi)
  (:export #:start-repl)
  (:documentation "REPL"))
(defpackage :world.events
  (:use :cl :jscl/ffi)
  (:export #:make-event
           #:dispatch-event)
  (:documentation "Abstract events in the game tank"))
(defpackage :xhr
  (:use :cl :jscl/ffi)
  (:export #:xhr)
  (:documentation "XML HTTP Request wrappers"))
