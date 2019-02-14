(in-package :Tootsville)

;;; Copyright © 2009-2010, Bruce-Robert Pocock

(define-operator-command $ (words user plane) 
  "Execute a command script

    Usage: 
@example
#$ SCRIPT
@end example 

The script name must be a function previously defined by @samp{#SCRIPT}
    
"
  
  )
(define-operator-command addevent (words user plane) 
  "Add a GameEvent to a Zone

    Add a GameEvent to a Zone. Must have staff level 4 (DESIGNER) to use
    this command.

    Syntax for use: 
@example
#addevent [EVENTNAME]
@end example 

    Examples
@example 
    #addevent LaserTagGame
    #addevent PropsWeather
    #addevent ShaddowFalls
    #addevent Tootlympics
@end example
"
  
  )

(define-operator-command agent (words user plane) "

    WRITEME: Document this method brpocock@star-hope.org

    "
                         
                         )
(define-operator-command askme (words user plane) "
                     
WRITEME
"
                         
                         )
(define-operator-command ban (words user plane) "
                   
    Ban a user  permanently. Must have staff level 2  (MODERATOR) to use
    this command.

    Syntax for use
@example 
    #ban [REASONCODE] [LOGIN]
@end example

    Examples
@example 
    #ban obs.rude pil
@end example 

See Also: `OPERATOR-KICK'"
                         
                         )
(define-operator-command banhammer (words user plane) 
  "Ban an IP address from connecting.

Bans can be listed using @samp{#banhammer #list}

Bans  can  be lifted  using  @samp{#banhammer  #-ip IP-ADDRESS}  (or 
hostname)

A  ban  can   be  placed  with  @samp{#banhammer   #+ip  IP-ADDRESS}  or
@samp{#banhammer  #+ip HOSTNAME}  or  @samp{#banhammer #user  USERNAME}.
In the  latter case, the  user's connected IP  address is used.  This is
expected to be the most common usage.

Parameters:  the  first  word  is  a  subcommand;  one  of  @samp{#+ip},
@samp{#-ip},   @samp{#user},    or   @samp{#list}.    For   @samp{#+ip},
@samp{#-ip}, or @samp{#user}, an additional parameter is needed."
  
  )
(define-operator-command beam (words user plane) "
                    throws org.json.JSONException,
                           PrivilegeRequiredException

    Beam yourself to a different room.

    Syntax for use
    #beam [ROOM]

    Examples
    #beam tootSquare

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        org.json.JSONException - if the data can't be formatted for some reason 
        PrivilegeRequiredException - if so
"
                         
                         )
(define-operator-command build (words user plane) "

    Create  a new  room  in  the current  zone.  Must  have staff  level
    8 (DEVELOPER) to use this command.

    Syntax for use
    #spawnroom [MONIKER] [TITLE] [SWF]
    #spawnroom [MONIKER] [TITLE]

    NOTE: Uses tootCastleJoust.swf as default. This can be set after the
    room has been created by setting the 'f' room variable.

    Examples
    #spawnroom tootCastleJoust2 Joust2 tootCastleJoust.swf
    #spawnroom tootCastleJoust2 Joust2

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command census (words user plane) "
                      throws PrivilegeRequiredException

    Simply reference  a range of  users, for testing purposes.  Takes an
    optional low and high point, or runs 0…250000. (250,000) This will
    assert free or  paid member status, restore default  free items, and
    seriously strain the caché and database subsystems.

    Parameters:
        words - optional low and high points of the range to be referenced.
        u - God
        room - God's room 
    Throws:
        PrivilegeRequiredException - if someone other than God tries to call this routine
"
                         
                         )
(define-operator-command clearbadge (words user plane) "

    Clear badges off of the map interface. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #clearbadge [LOGIN] [ROOM].
    Login

        username of a character
        #me for the character you are logged in as

    Room

        room moniker of a room
        #here for the room you are currently in
        #all for every room

    Examples
    #clearbadge snowcone tootSquare
    #clearbadge snowcone #all
    #clearbadge snowcone #here
    #clearbadge #me #all
    #clearbadge #me #here

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command clearcache (words user plane)"
                          throws com.whirlycott.cache.CacheException,
                                 PrivilegeRequiredException

    Forcibly clear all cachÃ©s

    Parameters:
        words - none
        u - operator
        r - room operator is in 
    Throws:
        com.whirlycott.cache.CacheException - if the cachÃ© subsystem throws an exception 
        PrivilegeRequiredException - if (u) is not a developer
"
                         
                         )
(define-operator-command clearevent (words user plane) "

    Clear a GameEvent from a Zone. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #clearevent [EVENTNAME]

    Examples
    #clearevent LaserTagGame
    #clearevent PropsWeather
    #clearevent ShaddowFalls
    #clearevent Tootlympics

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_addevent(String[], AbstractUser, Room), op_getevents(String[], AbstractUser, Room)
"

)
(define-operator-command clearvar (words user plane) "

    Clear a room variable. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #clearvar @[ROOM] [VARIABLE] [VALUE]
    #clearvar [VARIABLE] [VALUE]

    See op_setvar(String[], AbstractUser, Room) to set a variable.

    Examples
    #clearvar @tootsSquareWest anim~ropes 2
    #clearvar anim~ropes 2

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_setvar(String[], AbstractUser, Room), op_getvar(String[], AbstractUser, Room)
"

)
(define-operator-command cloneroom (words u plane)"
                         throws PrivilegeRequiredException

    clone a room

    Parameters:
        words - moniker of the new room
        u - user cloning the room
        r - room to be cloned, in which the user must currently be standing 
    Throws:
        PrivilegeRequiredException - if the user doesn't have Designer level privileges, at least
"

                         )
(define-operator-command createroom (words user plane) "
                          throws PrivilegeRequiredException,
                                 NotReadyException

    WRITEME: Document this metho WRITEMEd brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME 
        NotReadyException - WRITEME
"

)
(define-operator-command dbcpinfo (words user plane) "
                        throws PrivilegeRequiredException,
                               NotFoundException,
                               SQLException

    Get DBCP information. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #dbcpinfo

    Examples
    #dbcpinfo

    Parameters:
        words - ignored
        u - user requesting the info
        room - the room in which the user is standng 
    Throws:
        PrivilegeRequiredException - WRITEME 
        NotFoundException - WRITEME 
        SQLException - WRITEME
"

)
(define-operator-command dress (words user plane) "
                     throws NumberFormatException,
                            DataException

    Force a character to wear a specific clothing item. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #dress [LOGIN] [ITEM] [optional: COLOUR]

    Examples
    #dress flappyperry 1337

    Parameters:
        words - login name, item ID, and optional colour string (see Colour.Colour(String))
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        DataException - if the colour is bad 
        NumberFormatException - if the colour is bad
"
                            
)
(define-operator-command drop (words user plane) "

    find an item in your inventory based upon the item ID # and destroy (drop) it

    Parameters:
        words - the item# to be destroyed
        u - the user dropping the item
        room - the room in which the user is standing
"

)
(define-operator-command dropkick (words user plane) "

    Silently remove the named user from the game by disconnection. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #dropkick [LOGIN]

    Examples
    #dropkick flappyperry

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command dumpthreads (words user plane) "

    Dump debugging information including all running threads to a server-side file. Must have staff level 1 (STAFF) to use this command.

    Syntax for use
    #dumpthreads

    Examples
    #dumpthreads

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command enablepathfinder (words user plane) "

    Temporary test routine for testing pathfinders on users

    Parameters:
        words - true or false
        u - who
        room - where
"

)
(define-operator-command evacuate (words user plane) "

    Evacuate all users from your current Zone into another Zone. Will error if the Zone specified does not exist. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #evacuate [ZONE]

    Examples
    #evacuate dottie

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command filter (words user plane) "

    Parameters:
        words - see op_testcensor(String[], AbstractUser, Room)
        u - see op_testcensor(String[], AbstractUser, Room)
        room - see op_testcensor(String[], AbstractUser, Room)
    See Also:
        op_testcensor(String[], AbstractUser, Room)
"

)
(define-operator-command finger (words user plane) "

    Finger a user account. Return interesting details in an administrative message. Must have staff level 1 (STAFF) to use this command.

    Syntax for use
    #finger [LOGIN]

    Examples
    #finger mouser

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command flush (words user plane) "

    Attempt to flush the pending database records to the database (if any). Reports back how many remain pending after the sweep. Does not affect the ongoing background sweep process, which will continue to run normally.

    Parameters:
        words - optionally, a single integer defining how many records to attempt to flush. Defaults to 100.
        u - operator
        room - ignored
"

                            )
(define-operator-command game (words user plane) "

    Send a command into the operator command interpreter for a running game (if that game provides one)

    Usage: #game gameClass (strings...)

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing. The GameEvent must be attached thereunto.
"

)
(define-operator-command getconfig (words user plane) "

    Get a Appius configuration variable. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #getconfig [PROPERTY]

    Examples
    #getconfig org.starhope.appius.requireBeta

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command getevents (words user plane) "

    List GameEvents in your current Zone. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #getevents

    Examples
    #getevents

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_addevent(String[], AbstractUser, Room), op_getevents(String[], AbstractUser, Room)
"

)
(define-operator-command getmotd (words user plane) "

    Retrieve the current Message Of The Day as a server message

    Parameters:
        words - ignored
        u - user placing request
        room - room in which the user is standing
"

)
(define-operator-command getschedule (words user plane) "
                           throws PrivilegeRequiredException

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME
"

)
(define-operator-command getschedulefor (words user plane) "
                              throws PrivilegeRequiredException,
                                     ClassNotFoundException

    Get scheduled events for a particular class (scheduled by that class)

    Parameters:
        words - Specify the class's full, canonical name
        u - the user invoking
        room - the room in which the user is standing 
    Throws:
        PrivilegeRequiredException - if the user doesn't have at least moderator privilege level 
        ClassNotFoundException - is the class requested can't be found (probably a typo)
"

)
(define-operator-command getuvar (words user plane) "

    Get a user variable. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #getuvar [LOGIN] [VARIABLE]
    User Name

        user name of a character
        #me for the user you are logged in as

    Examples
    #getuvar mouser d
    #getuvar #me d

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_setuvar(String[], AbstractUser, Room), op_getuvars(String[], AbstractUser, Room)
"

                         )
(define-operator-command getuvars (words user plane) "

    Get all user variables for a given user. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #getuvars [LOGIN].
    User Name

        user name of a character
        #me for the user you are logged in as

    Examples
    #getuvars mouser
    #getuvars #me

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_setuvar(String[], AbstractUser, Room), op_getuvar(String[], AbstractUser, Room)
"

)
(define-operator-command getvar (words user plane) "

    Get a room variable. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #getvar @[ROOM] [VARIABLE]
    #getvar [VARIABLE]

    Examples
    #getvar @tootsSquareWest anim~ropes
    #getvar anim~ropes

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_setvar(String[], AbstractUser, Room), op_clearvar(String[], AbstractUser, Room), op_getvars(String[], AbstractUser, Room)
"

)
(define-operator-command getvars (words user plane) "

    Get all room variables. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #getvars [ROOM]
    #getvars

    Examples
    #getvars tootsSquare
    #getvars

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_setvar(String[], AbstractUser, Room), op_clearvar(String[], AbstractUser, Room), op_getvar(String[], AbstractUser, Room)
"

                              )
(define-operator-command give (words u plane)"
                    throws NumberFormatException,
                    org.json.JSONException,
                           AlreadyExistsException

    give a gift

    usage: #give ITEM# USER
    WRITEME

    Parameters:
        words - item# to be given, and recipient
        u - the giver
        r - room in which the giver is standing 
    Throws:
        NumberFormatException - if the item# given is not a number 
        org.json.JSONException - WRITEME 
        AlreadyExistsException - WRITEME
"

                         )
(define-operator-command givehead (words user plane) "
                        throws PrivilegeRequiredException

    Give an inventory item to a user. Must have staff level 1 (STAFF) to use this command.

    NOTE: #grant and #givehead are identical.

    Syntax for use
    #givehead [ITEM] [LOGIN]
    #grant [ITEM] [LOGIN]

    Examples
    #givehead 1337 louis
    #grant 1337 louis

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        PrivilegeRequiredException - requires staff level permissions
"

)
(define-operator-command goto (words user plane) "
                    throws PrivilegeRequiredException

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME
"

                           )
(define-operator-command grant (words user plane) "
                     throws PrivilegeRequiredException

    Grant an item to a user. See op_givehead(String[], AbstractUser, Room)

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        PrivilegeRequiredException - if the user doesn't have sufficient privileges
"
                         
                         )
(define-operator-command headcount (words user plane) "

    Get headcount information about the running system. Must have staff level 1 (STAFF) to use this command.

    Syntax for use
    #headcount #all
    #headcount #members
    #headcount #room

    Examples
    #headcount #all
    #headcount #members
    #headcount #room

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
See Also:

        headcount_all(AbstractUser,                               Room)

        headcount_highwater(AbstractUser,                         Room)

        headcount_rooms(AbstractUser,                             Room)

        headcount_members(AbstractUser, Room)
"
                         
                         )
(define-operator-command inv (words user plane) "
                   throws PrivilegeRequiredException

    Get inventory  items for  a particular user.  By default,  this will
    bring up  only the active  items â€”  e.g. clothing being  worn, and
    so forth.

    To get all active inventory for an user: #inv LOGIN

    To get  all inventory for an  user, active or inactive  (this may be
    very long!): #inv LOGIN #all

    To  get inventory  of a  particular type,  active or  inactive: #inv
    LOGIN #type TYPE

    The    type    strings    accepted    are    those    accepted    by
    Commands.do_getInventoryByType(JSONObject,  AbstractUser,   Room)  ;
    this means that both the  $SPECIFIC-TYPE and TYPE-SET-NAME forms are
    accepted. The list  of specific types might include  e.g. $Hair, and
    a  type-set-name  might  be  something like  clothing.  The  set  of
    available type-set-names is specified in the configuration file.

    Parameters:
        
words - User  name, and optional tag  #all to show all  items instead of
just  active, or  optional tag  #type  and a  type string  for items  of
a     specific      type.     For      item     types,      refer     to
Commands.do_getInventoryByType(JSONObject, AbstractUser,  Room) â€” note
that this supports only a single item type (or type-set)
        
u - user calling this command
        
room - the room in which that user is standing 
    
Throws:
        PrivilegeRequiredException - if the user lacks staff privileges to invoke this command
"

)
(define-operator-command kick (words user plane) "
                    throws NotFoundException

    Kick a user offline for a certain reason

    Kick a user offline for a certain reason. Must have staff level 2 (MODERATOR) to use this command.

    Syntax for use: #kick [REASONCODE] [LOGIN]

    Examples
    #kick obs.rude pil

    Reason Codes:

        PER.MAIL = Don't share personal information like eMail addresses!
        PER.NAME = Don't share personal information like your real name!
        PER.PASS = Don't share personal information like passwords!
        PER.CHAT = Don't share personal information like chat and instant messaging \ information!
        PER.LOCA = Don't share personal information like your location!
        PER.AGES = Don't share personal information like your age!
        PER.BDAY = Don't share personal information like your birth date!
        BUL.MEAN = Don't be mean!
        OBS.RUDE = Don't be rude!
        OBS.FOUL = Don't use foul words!
        NET.CHTR = No cheating!
        APP.PARN = You need your parent's permission in order to chat in Tootsville.
        APP.MAIL = You need to confirm your eMail address in order to chat in Tootsville.
        APP.AGES = Lying about your birth date is against the law!

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        NotFoundException - if the warning reason code is not valid
    See Also:
        op_warn(String[], AbstractUser, Room), op_ban(String[], AbstractUser, Room)
"

                           )
(define-operator-command king (words user plane) "
                    throws org.json.JSONException

    Apply  a  gift  membership  to   an  user.  Must  have  staff  level
    4 (DESIGNER) to use this command.

    Syntax for use
    #king [DAYS] [LOGIN]

    Examples
    #king 2 flappyperry

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        org.json.JSONException - if something can't be cast into a JSON packet underlying
"

)
(define-operator-command liftban (words user plane) "
                       throws PrivilegeRequiredException,
                              NotFoundException

    Lift the ban upon a user. Must have staff level 2 (MODERATOR) to use
    this command.

    NOTE: In order  to un-ban a user,  you must key in  the literal word
    “yes” as  the third parameter,  and supply the ban  reason as
    the first. This is to avoid accidentally lifting a ban.

    Syntax for use
    #liftban [BANREASON] [USER] yes

    Examples
    #liftban app.mail flappyperry yes

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        PrivilegeRequiredException - if the user lacks PrivilegeRequiredException 
        NotFoundException - if the warning reason code is not valid
"

)
(define-operator-command loadlists (words user plane) "

    Reload the censorship lists. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #loadlists

    Examples
    #loadlists

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command mem (words user plane) "

    Display some memory usage and other debugging type information as an pop-up message. Must have Designer privileges to use this command.

    Syntax for use
    #mem

    Examples
    #mem

    Parameters:
        words - command parameters (not used)
        u - invoking user
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

                         )
(define-operator-command metronome (words user plane) "

    Display information  about or  micromanage the metronome.  Must have
    staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #metronome [OPTION]

    Options

        #rate - Displays a message indicating the rate that the metronome ticks in milliseconds.
        #last - Displays a message indicating the time in milliseconds when the last metronome tick occured.
        #start - Starts the metronome.
        #stop - Stops the metronome.
        #restart - Restarts the metronome.
        #tick - Forces the metronome to tick.

    Examples
    #metronome #rate
    #metronome #last
    #metronome #start
    #metronome #stop
    #metronome #restart
    #metronome #tick

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

                                )
(define-operator-command motd (words user plane) "

    Set the  message of the day.  Must have staff level  4 (DESIGNER) to
    use this command.

    Syntax for use
    #motd [MESSAGE...]

    Examples
    #motd I am setting the message of the day

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command mute (words user plane) "

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_stfu(String[], AbstractUser, Room)
"

)
(define-operator-command nuke (words user plane) "
                    throws PrivilegeRequiredException

    Forcibly disconnect everyone in a room.

    Parameters:
        words - the name of the room to be nuked
        u - The user (operator) executing this instruction
        room - The room to be nuked 
    Throws:
        PrivilegeRequiredException - WRITEME
"

)
(define-operator-command parentapproves (words user plane) "
                              throws PrivilegeRequiredException,
                                     GameLogicException,
                                     ForbiddenUserException,
                                     AlreadyExistsException,
                                     NotReadyException

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME 
        GameLogicException - WRITEME 
        ForbiddenUserException - WRITEME 
        AlreadyExistsException - WRITEME 
        NotReadyException - WRITEME
"

)
(define-operator-command ping (words user plane) "

    Ping the  server, to force  a neutral administrative  message reply.
    Must have staff level 1 (STAFF) to use this command.

    Syntax for use
    #ping

    Examples
    #ping

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
                           
)
(define-operator-command place (words user plane) "
                     throws PrivilegeRequiredException

    Add a  Place to  a room.  This command supports  the basic  types of
    event Places,  and adds them to  the room in the  given WHERE place.
    WHERE can be  a diamond-shaped area around the  operator issuing the
    command  (using  #here, #here-tiny,  or  #here-big),  or can  be  an
    explicitly-issued  polygon  string.  The  event region  ID  will  be
    automatically assigned.

     Usage:
     #place WHERE #item ITEM-NUMBER
     #place WHERE #room MONIKER
     #place WHERE #vitem PAID-ITEM-NUMBER
     #place WHERE #item2 ITEM-NUMBER PAID-ITEM-NUMBER
     #place WHERE #exit MONIKER
     #place WHERE #mini MINIGAME-MONIKER
     #place WHERE #walk
     WHERE := #here | #here-tiny | #here-big | x,y~x,y~x,y~x,y polygon list
     

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME
"

)
(define-operator-command purgephysics (words user plane) "
                            throws PrivilegeRequiredException

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME
"

                         )
(define-operator-command push (words user plane) "

    WRITEME

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME
"
                         
                         )
(define-operator-command put (words user plane) "
                   throws PrivilegeRequiredException

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME
"

                         )
(define-operator-command rc (words user plane) "
                  throws PrivilegeRequiredException,
                         InstantiationException,
                         IllegalAccessException,
                         ClassNotFoundException

    Run   an   RC  (RunCommands)   script.   Both   the  â€œsystem   run
    commandsâ€  (â€œrunâ€)  method  and   the  â€œnew  zone  run
    commandsâ€ (â€œnewZoneâ€) method will be executed; the

    Parameters:
        words - class name
        u - user
        room - room 
    Throws:
        PrivilegeRequiredException - user must be dev level 
        InstantiationException - class can't instantiate 
        IllegalAccessException - class can't instantiate 
        ClassNotFoundException - probably a typo on class name
"

)
(define-operator-command reboot (words user plane) "

    Forces appius to restart. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #reboot

    Examples
    #reboot

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command reloadconfig (words user plane) "

    Reloads configuration properties. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #reloadconfig

    Examples
    #reloadconfig

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command retire (words user plane) "

    Forces a  zone to retire.  This will disconnect anyone  currently in
    the zone.  Use #evacuate to  move users  to another zone.  Must have
    staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #retire [ZONE]
    #retire

    Examples
    #retire dottie
    #retire

    Parameters:
        
words  - The  command parameter  s (whitespace-delimited  list) provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command run (words
                              u r)"
  throws ClassNotFoundException,
                          InstantiationException,
                          IllegalAccessException

    Run an arbitrary Java routine through an uploaded Runnable or RunCommands class

    Parameters:
        words - WRITEME
        who - WRITEME
        where - WRITEME 
    Throws:
        ClassNotFoundException - WRITEME 
        InstantiationException - WRITEME 
        IllegalAccessException - WRITEME
"

                              )
(define-operator-command saveroomvars (words user plane) "

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME
"

)
(define-operator-command scotty (words user plane) "
                      throws org.json.JSONException,
                             PrivilegeRequiredException

    Forces a user into another room. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #scotty [LOGIN] [ROOM]

    Examples
    #scotty mouser tootSquareWest

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        org.json.JSONException - if something can't be done 
        PrivilegeRequiredException - if so
"

)
(define-operator-command setavatarcolors (words user plane) "
                               throws NumberFormatException,
                                      DataException
                                      
    Sets the base an extra color of a user's avatar. Colors should be passed in HTML format (see below). Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #setavatarcolors [LOGIN] [BASE] [EXTRA]

    Instantiate a Colour object based upon the CSS, HTML, or JSON style of colour string.

        The \"CSS style\" uses a decimal triplet in the form rgb(R,G,B) (the literal string \"rgb(\" identifies it).
        The \"HTML style\" uses a # sign followed by either 3 or 6 hex characters, in the form #RGB or #RRGGBB.

    Examples
    #setavatarcolors mouser #000000 #ffffff
    #setavatarcolors mouser rgb(0,0,0) rgb(255,255,255)

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances. 
    Throws:
        DataException - if the colour is bad 
        NumberFormatException - if the colour is bad
    See Also:
        Colour.Colour(String)
"

)
(define-operator-command setbadge (words user plane) "

    Set the badge on a room. Must have staff level 4 (DESIGNER) to use this command on another character. Staff level 2 (MODERATOR) can use the command with the #me parameter.

    NOTE: Rooms that don't directly appear on the map will not have visible badges, but the badges can still be set.

    Syntax for use
    #setbadge [LOGIN] [ROOM] #setbadge
    Login

    User name of a character
        #me for the character you are logged in as

        Room

        Room moniker of a room
        #here for the room you are currently in

    NOTE: Using #setbadge with no parameters will assume default values which are identical to typing #setbadge #me #here

    Examples
    #setbadge snowcone tootSquareWest
    #setbadge #me tootSquare
    #setbadge snowcone #here
    #setbadge #me #here

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

)
(define-operator-command setconfig (words user plane) "

    Set a config property. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #setconfing [PROPERTY] [VALUE]

    Examples
    #setconfig org.starhope.appius.requireBeta true

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_getconfig(String[], AbstractUser, Room)
"

)
(define-operator-command setstafflevel (words user plane) "
                             throws PrivilegeRequiredException,
                                    GameLogicException

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME 
    Throws:
        PrivilegeRequiredException - WRITEME 
        GameLogicException - WRITEME
"

)
(define-operator-command setuvar (words user plane) "

    Set a user variable. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #setuvar @[LOGIN] [VARIABLE] [VALUE...] #setuvar [VARIABLE] [VALUE...] #setbadge

    NOTE: Using #setconfig without an @[LOGIN] parameter will apply the changes to the user issuing the command.

    Examples
    #setuvar @mouser d = 254~376~254~376~SE~1267735566759
    #setuvar d = 254~376~254~376~SE~1267735566759

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_getuvar(String[], AbstractUser, Room), op_getuvars(String[], AbstractUser, Room)
"

)
(define-operator-command setvar (words user plane) "

    Set a room variable. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #setvar #replace [VARIABLE] [FIND] [REPLACE]
    #setvar @[ROOM] [VARIABLE] [VALUE...]
    #setvar [VARIABLE] [VALUE...]

    WARNING: SETTING ROOM VARIABLES TO INVALID VALUES CAN CAUSE UNEXPECTED RESULTS. DOUBLE CHECK ALL VALUES BEING SET FOR CORRECTNESS.

    Use #replace to change a room variable from one value to another.

    Examples
    #setvar @tootsSquareWest anim~ropes 2
    #setvar anim~ropes 2

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
    See Also:
        op_clearvar(String[], AbstractUser, Room), op_getvar(String[], AbstractUser, Room), Room.setVariable(String, String)
"

)
(define-operator-command shanghai (words user plane) "
                        throws org.json.JSONException

    Force a client into a different room and zone

    Parameters:
        words - space-delimited: USER ZONE ROOM
        u - the user doing the Shanghai:ing
        room - the room where the kidnapper is 
    Throws:
        org.json.JSONException - if something can't work in JSON
"

)
(define-operator-command shout (words user plane) "

    Speak in another zone. This is intended for using operator commands in a remote zone, not normal chat messages. Must have staff level 2 (MODERATOR) to use this command.

    Syntax for use
    #shout [ZONE] [ROOM] [COMMAND...]

    Examples
    #shout dottie tootSquareWest #wall Hello Everyone
    #shout dottie tootSquare #retire

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"

                            )
(define-operator-command spawnzone (words user plane) "

    Create a new zone. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #spawnzone [ZONE]

    Examples
    #spawnzone Cupcake

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command speak (words user plane) "

    Allows a user to speak. Must have staff level 2 (MODERATOR) to use this command.

    Syntax for use
    #speak [LOGIN]

    Examples
    #speak flappyperry

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command stfu (words user plane) "

    Silences a user. Must have staff level 2 (MODERATOR) to use this command.

    Syntax for use
    #stfu [LOGIN]

    Examples
    #stfu flappyperry

    Parameters:
        words - The command parameters (whitespace-delimited list) provided after the # command name
        u - The user invoking the operator command
        room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command testcensor (words user plane) "

    Test a message with the censor, displays the filter result.

    Syntax for use
    #testcensor [MESSAGE]

    Examples
    #testcensor This message will be filtered and the result will be displayed.

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command time (words user plane) "

    Displays a message  with the current time in  Eastern Standard Time.
    Must have staff level 1 (STAFF) to use this command.

    Syntax for use
    #time

    Examples
    #time

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command unbuild (words user plane) "
                       throws NotFoundException

    Destroys a room. Must have staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #unbuild [ROOM]

    Examples
    #unbuild tootUniversity

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
Throws:
        NotFoundException - if the room doesn't exist
"
                         
                         )
(define-operator-command v (words user plane) "
                 throws org.json.JSONException,
                        NotFoundException

    Forces a user  to say a message. Must have  staff level 4 (DESIGNER)
    to use this command.

    Syntax for use
    #v [LOGIN] [MESSAGE...]

    Examples
    #v flappyperry I like to cause trouble in tootsville

    Parameters:

        words  -  The  command  parameters  (whitespace-delimited  list)
        provided after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
Throws:
        org.json.JSONException - if the speech can't be represented in JSON somehow 
        NotFoundException - WRITEME
    See Also:
        Commands.do_speak(JSONObject, AbstractUser, Room)
"

)
(define-operator-command verbosebugs (words u plane)"

    Set verbose bug backtrace reporting on or off

    Parameters:
        words - single word \"true\" or \"false\"
        u - the user affected
        r - the room in which the user is standing
"
                         
                         )
(define-operator-command wall (words user plane) "

    Sends an pop-up message to everyone in the zone. Must have staff level 4 (DESIGNER) to use this command.

    Syntax for use
    #wall [MESSAGE...]

    Examples
    #wall This message will go to everyone in the zone I am in.

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command wallops (words user plane) "

    Sends an pop-up message to all  staff members in the zone. Must have
    staff level 2 (MODERATOR) to use this command.

    Syntax for use
    #wallops [MESSAGE...]

    Examples
    #wallops This message will go to all other staff members in this zone.

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command wallzones (words user plane) "

    Sends an  pop-up message to  all everyone  in every zone.  Must have
    staff level 8 (DEVELOPER) to use this command.

    Syntax for use
    #wallzones [MESSAGE...]

    Examples
    #wallzones This message will go to everyone in every zone.

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command warn (words user plane) "
                    throws NotFoundException

    Warn a user about breaking a rule. Must have staff level 2 (MODERATOR) to use this command.

    Syntax for use: #warn [REASONCODE] [LOGIN]

    Examples
    #warn obs.rude pil

    Reason Codes:

        PER.MAIL = Don't share personal information like eMail addresses!
        PER.NAME = Don't share personal information like your real name!
        PER.PASS = Don't share personal information like passwords!
        PER.CHAT = Don't share personal information like chat and instant messaging \ information!
        PER.LOCA = Don't share personal information like your location!
        PER.AGES = Don't share personal information like your age!
        PER.BDAY = Don't share personal information like your birth date!
        BUL.MEAN = Don't be mean!
        OBS.RUDE = Don't be rude!
        OBS.FOUL = Don't use foul words!
        NET.CHTR = No cheating!
        APP.PARN = You need your parent's permission in order to chat in Tootsville.
        APP.MAIL = You need to confirm your eMail address in order to chat in Tootsville.
        APP.AGES = Lying about your birth date is against the law!

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
Throws:
        NotFoundException - if the warning reason code is not valid
    See Also:
        op_kick(String[], AbstractUser, Room), op_ban(String[], AbstractUser, Room)
"
                         
                         )
(define-operator-command whatis (words user plane) "
                      throws NumberFormatException,
                             NotFoundException

    Displays information about an item.  Must have staff level 1 (STAFF)
    to use this command.

    Syntax for use
    #whatis [ITEM]

    Examples
    #whatis 1337

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
Throws:
        NotFoundException - WRITEME 
        NumberFormatException - WRITEME
"
                         
                         )
(define-operator-command whereami (words user plane) "

    Return an administrative message with the  name of the Zone in which
    the player is currently standing. Must have staff level 1 (STAFF) to
    use this command.

    Syntax for use
    #whereami

    Examples
    #whereami

    Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         
                         )
(define-operator-command whereis (words user plane) "

    Find  out in  what what  room a  character is  standing, if  s/he is
    logged in at the moment. Must  have staff level 2 (MODERATOR) to use
    this command.

    Syntax for use
    #whereis [LOGIN]
    Login

        User Name of a specific user
        #everyone for a the location of every user in the zone.
        @[ROOM] for the location of every user in the specified room.

    Examples
    #whereis snowcone
    #whereis #everyone
    #whereis @tootSquare

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
See Also:
        whereis_atRoom(AbstractUser, Room, String), whereis_everyone(AbstractUser, Room)
"
                         
                         )
(define-operator-command who (words user plane) "
                   throws PrivilegeRequiredException,
                          NotFoundException

    Displays a  list of everyone  currently in  a room. Must  have staff
    level 2 (MODERATOR) to use this command.

    Syntax for use
    #who [ROOM]
    #who

    NOTE: Leaving off the ROOM  parameter will default to displaying for
    the room the command was initialized in.

    Examples
    #who tootSquare
    #whereis

    Parameters:
        
words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
    
Throws:
        PrivilegeRequiredException - if the user calling isn't a staff member 
        NotFoundException - if the chosen room does not exist
"
                         
                         )
(define-operator-command whoami (words user plane) "

    Cause  the character  to speak  his/her  name in  the current  room.
    Appears as  dialogue in the  form: â€œHello, my name  is NAMEâ€.
    Must have staff level 1 (STAFF) to use this command.

    Syntax for use
    #whoami

    Examples
    #whoami

    Parameters: 
 
words  - The  command  parameters  (whitespace-delimited list)  provided
        after the # command name

        u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         )
(define-operator-command whoareyou (words user plane) "

    Ask  the  server who  it  is.  This  command should  return  version
    information on some of the critical classes used in the game server.
    Must have staff level 2 (MODERATOR) to use this command.

    Syntax for use
    #whoareyou

    Examples
    #whoareyou

    Parameters: 
 
words  - The  command  parameters  (whitespace-delimited list)  provided
        after the # command name
        
u - The user invoking the operator command
        
room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
                         )
(define-operator-command zoom (words user plane) "

    WRITEME: Document this method brpocock@star-hope.org

    Parameters:
        words - WRITEME
        u - WRITEME
        room - WRITEME
"
                         )
