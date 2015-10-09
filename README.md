
# README #

## Table of contents ##
1. About
2. Installation
3. Configuration
4. Usage
5. Shop mode
6. Dependencies
7. Author
8. Links
9. Special thanks

## About ##
TMWChat is a chat client for The Mana World MMORPG, running in Emacs. It supports
sending and receiving messages on the world map, sending and receiving
private messages (whispers), and emotes. A simple notification system allows
the user to see a pop-up in the notification area, when Emacs window is not active.
It also supports ManaPlus-compatible shop mode.

## Installation ##
Add this to your Emacs init file:

```
#!lisp
(add-to-list 'load-path "/path/to/tmwchat/directory")
(require 'tmwchat)
```

## Configuration ##
If the module is loaded properly, you can use Emacs' customization system to
set some settings. Do it by running

```
M-x customize-group tmwchat
```

To configure notification module settings:

```
M-x customize-group todochiku
```

## Usage ##
After setting up and configuring TMWChat, you log in by typing

```
M-x tmwchat
```

If you want to want to see the nearby/recent/online users list, simply
execute the command
```
M-x speedbar
```

It automatically logs you into The Mana World game server (or any other
TMWA-compatible server you specify). If  no errors occur, you can start
chatting with your Mana-friends. There are few built-in commands

```
/help --show help message
/w NickName message
/w "NickName with spaces" message -- send a personal message to NickName
/emote <number> -- show emote (check source code for possible emote codes)
/emotes -- show emote codes
/party <message> -- send message to your party
/mute -- mute notification sounds
/unmute -- play notification sounds
/room -- list players near you
/online -- show online players list
/away [optional afk message] -- away from keyboard
/back -- you are back!
/sit -- sit down
/stand -- stand up
/turn left|right|up|down -- turn in given direction
/block NickName -- block player (don't receive whispers, public chats, emotes)
/equip ItemID  -- equip item ID
/inv -- show inventory
/zeny -- show money
/dc -- disconnect
Any other line simply acts as a command to send a chat message in public chat.
```

Tab-completion for nicknames is supported.

## Shop mode ##
If you enable shop mode in settings, your chat client can act as a shop.
In the customization window you need to add the item IDs, price and amount
you want to buy and sell. After this, the following commands will work
(you need to whisper them to the shop char):

```
!selllist -- show the list of items that the shop sells. This command is automatically
             sent by ManaPlus when you rightclick => Buy
!buylist  -- show the list of items that the shop buys. This command is automatically
             sent by ManaPlus when you rightclick => Sell
!buyitem ID PRICE AMOUNT  -- buy items from shop. Automatically sent by ManaPlus.
!sellitem ID PRICE AMOUNT -- sell item to shop. Automatically sent by ManaPlus.
!money [AMOUNT] -- retrieve money from shop. With this command you can also
                   transfer items and money to the shop. If AMOUNT is omitted,
		   retrieve all money from shop. This command works only if the
		   person whispering to shop is set as shop admin in settings.
```

## Dependencies ##
* Emacs version 24.4 or higher
* todochiku version 0.8.0 (included)
* Windows users: Snarl notification daemon (see download link below)

## Author ##
Joseph Botosh <rumly111@gmail.com> (TMW nickname: Travolta)

## Links ##
* Project homepage: https://bitbucket.org/rumly111/tmwchat
* The Mana World homepage: https://www.themanaworld.org
* CrazyTree GitHub: https://github.com/pclouds/crazytree
* Emacs homepage: https://www.gnu.org/software/emacs
* Snarl download: http://sourceforge.net/projects/snarlwin

## Special thanks ##
* The Mana World Development Team
* pclouds, the author of CrazyTree TMW-bot, the source of inspiration for TMWChat
