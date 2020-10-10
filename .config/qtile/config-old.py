import os
import socket
import subprocess
from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.utils import guess_terminal
from libqtile.core.manager import Qtile

mod = "mod4"
myTerm = guess_terminal()
browser = "firefox"

keys = [
    # The essentials
    Key([mod], "Return", lazy.spawn(myTerm), desc="Launches My Terminal"),
    Key(
        [mod, "shift"],
        "Return",
        lazy.spawn("dmenu_run -p 'Run: '"),
        desc="Dmenu Run Launcher",
    ),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle through layouts"),
    Key(
        [mod, "shift"],
        "Tab",
        lazy.previous_layout(),
        desc="Toggle backwards through layouts",
    ),
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill active window"),
    Key([mod, "shift"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    # Treetab controls
    Key(
        [mod, "control"],
        "k",
        lazy.layout.section_up(),
        desc="Move up a section in treetab",
    ),
    Key(
        [mod, "control"],
        "j",
        lazy.layout.section_down(),
        desc="Move down a section in treetab",
    ),
    # Window controls
    Key(
        [mod],
        "k",
        lazy.layout.down(),
        desc="Move focus down in current stack pane",
    ),
    Key(
        [mod],
        "j",
        lazy.layout.up(),
        desc="Move focus up in current stack pane",
    ),
    Key(
        [mod, "shift"],
        "k",
        lazy.layout.shuffle_down(),
        desc="Move windows down in current stack",
    ),
    Key(
        [mod, "shift"],
        "j",
        lazy.layout.shuffle_up(),
        desc="Move windows up in current stack",
    ),
    Key(
        [mod],
        "h",
        lazy.layout.grow(),
        lazy.layout.increase_nmaster(),
        desc="Expand window (MonadTall), increase number in master pane "
        + "(Tile)",
    ),
    Key(
        [mod],
        "l",
        lazy.layout.shrink(),
        lazy.layout.decrease_nmaster(),
        desc="Shrink window (MonadTall), decrease number in master pane "
        + "(Tile)",
    ),
    Key(
        [mod],
        "n",
        lazy.layout.normalize(),
        desc="normalize window size ratios",
    ),
    Key(
        [mod],
        "m",
        lazy.layout.maximize(),
        desc="toggle window between minimum and maximum sizes",
    ),
    Key([mod], "f", lazy.window.toggle_floating(), desc="toggle floating"),
    Key(
        [mod, "shift"],
        "m",
        lazy.window.toggle_fullscreen(),
        desc="toggle fullscreen",
    ),
    # Stack controls
    Key(
        [mod],
        "space",
        lazy.layout.next(),
        desc="Switch window focus to other pane(s) of stack",
    ),
    # Key([mod, "control"], "Return",
    #    lazy.layout.toggle_split(),
    #    desc='Toggle between split and unsplit sides of stack'
    # ),
    # My applications
    Key([mod], "a", lazy.spawn(browser), desc="Launch browser window"),
]

group_names = [
    ("CHAT", {"layout": "monadtall"}),
    ("PRGM", {"layout": "monadtall"}),
    ("CLAS", {"layout": "maximized"}),
    ("SCHL", {"layout": "monadtall"}),
    ("ENTM", {"layout": "maximized"}),
    ("MUSC", {"layout": "maximized"}),
]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    # Switch to another group
    keys.append(
        Key(
            [mod],
            str(i),
            lazy.group[name].toscreen(),
            desc="Switch to group " + name,
        )
    )
    # Send current window to another group & follow
    keys.append(
        Key(
            [mod, "shift"],
            str(i),
            lazy.windows.togroup(name, switch_group=True),
            desc="Switch to & move focused window to group " + name,
        )
    )
    # Send current window to another group
    keys.append(
        Key(
            [mod, "control"],
            str(i),
            lazy.window.togroup(name),
            desc="Move focused window to group " + name,
        )
    )

group_apps = {
    "CHAT": ("discord-canary", "element"),
    "PRGM": ("vscodium", "emacs"),
    "CLAS": ("zoom", "teams"),
    "SCHL": ("libreoffice", "vscodium"),
    "INET": (
        browser + " --new-window https://www.youtube.com/feed/subscriptions",
        browser + " --new-window https://mail.google.com/mail/u/0/",
    ),
    "MUSC": (
        "olivia",
        browser + " --new-window https://music.youtube.com/library/playlists",
    ),
}


layout_theme = {
    "border_width": 2,
    "margin": 6,
    "border_focus": "e1acff",
    "border_normal": "1d2330",
}

layouts = [
    # layout.MonadWide(**layout_theme),
    # layout.Bsp(**layout_theme),
    # layout.Stack(stacks=2, **layout_theme),
    # layout.Columns(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.Zoomy(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Tile(shift_windows=True, **layout_theme),
    layout.Stack(num_stacks=2),
    layout.TreeTab(
        font="Ubuntu",
        fontsize=10,
        sections=["FIRST", "SECOND"],
        section_fontsize=11,
        bg_color="141414",
        active_bg="90C435",
        active_fg="000000",
        inactive_bg="384323",
        inactive_fg="a0a0a0",
        padding_y=5,
        section_top=10,
        panel_width=320,
    ),
    layout.Floating(**layout_theme),
]

colors = [
    ("#292d3e", "#292d3e"),  # panel background
    ("#434758", "#434758"),  # background for current screen tab
    ("#ffffff", "#ffffff"),  # font color for group names
    ("#ff5555", "#ff5555"),  # border line color for current tab
    ("#8d62a9", "#8d62a9"),  # border line color for other tab and odd widgets
    ("#668bd7", "#668bd7"),  # color for the even widgets
    ("#e1acff", "#e1acff"),  # window name
]

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

widget_defaults = {
    "font": "Ubuntu Mono",
    "fontsize": 12,
    "padding": 2,
    "background": colors[2],
}
extension_defaults = widget_defaults.copy()

widgets_list = [
    widget.Sep(
        linewidth=0, padding=6, foreground=colors[2], background=colors[0]
    ),
    widget.GroupBox(
        font="Ubuntu Bold",
        fontsize=9,
        margin_y=3,
        margin_x=0,
        padding_y=5,
        padding_x=3,
        borderwidth=3,
        active=colors[2],
        inactive=colors[2],
        rounded=False,
        highlight_color=colors[1],
        highlight_method="line",
        this_current_screen_border=colors[3],
        this_screen_border=colors[4],
        other_current_screen_border=colors[0],
        other_screen_border=colors[0],
        foreground=colors[2],
        background=colors[0],
    ),
    widget.Prompt(
        prompt=prompt,
        font="Ubuntu Mono",
        padding=10,
        foreground=colors[3],
        background=colors[1],
    ),
    widget.Sep(
        linewidth=0, padding=40, foreground=colors[2], background=colors[0]
    ),
    widget.WindowName(foreground=colors[6], background=colors[0], padding=0),
    widget.TextBox(
        text="",
        background=colors[0],
        foreground=colors[4],
        padding=0,
        fontsize=37,
    ),
   #widget.TextBox(
   #    text=" ₿",
   #    padding=0,
   #    foreground=colors[2],
   #    background=colors[4],
   #    fontsize=12,
   #),
   #widget.BitcoinTicker(
   #    foreground=colors[2], background=colors[4], padding=5
   #),
    widget.TextBox(
        text="",
        background=colors[4],
        foreground=colors[5],
        padding=0,
        fontsize=37,
    ),
    widget.TextBox(
        text=" 🌡",
        padding=2,
        foreground=colors[2],
        background=colors[5],
        fontsize=11,
    ),
    widget.ThermalSensor(
        foreground=colors[2], background=colors[5], threshold=90, padding=5
    ),
    widget.TextBox(
        text="",
        background=colors[5],
        foreground=colors[4],
        padding=0,
        fontsize=37,
    ),
    widget.TextBox(
        text=" ⟳",
        padding=2,
        foreground=colors[2],
        background=colors[4],
        fontsize=14,
    ),
    widget.CheckUpdates(
        update_interval=1800,
        foreground=colors[2],
        mouse_callbacks={
            "Button1": lambda qtile: qtile.cmd_spawn(
                myTerm + " -e sudo pacman -Syu"
            )
        },
        background=colors[4],
    ),
    widget.TextBox(
        text="Updates",
        padding=5,
        mouse_callbacks={
            "Button1": lambda qtile: qtile.cmd_spawn(
                myTerm + " -e sudo pacman -Syu"
            )
        },
        foreground=colors[2],
        background=colors[4],
    ),
    widget.TextBox(
        text="",
        background=colors[4],
        foreground=colors[5],
        padding=0,
        fontsize=37,
    ),
    widget.TextBox(
        text=" 🖬",
        foreground=colors[2],
        background=colors[5],
        padding=0,
        fontsize=14,
    ),
    widget.Memory(
        foreground=colors[2],
        background=colors[5],
        mouse_callbacks={
            "Button1": lambda qtile: qtile.cmd_spawn(myTerm + " -e htop")
        },
        padding=5,
    ),
    widget.TextBox(
        text="",
        background=colors[5],
        foreground=colors[4],
        padding=0,
        fontsize=37,
    ),
    widget.Net(
        interface="enp6s0",
        format="{down} ↓↑ {up}",
        foreground=colors[2],
        background=colors[4],
        padding=5,
    ),
    widget.TextBox(
        text="",
        background=colors[4],
        foreground=colors[5],
        padding=0,
        fontsize=37,
    ),
    widget.TextBox(
        text=" Vol:", foreground=colors[2], background=colors[5], padding=0
    ),
    widget.Volume(foreground=colors[2], background=colors[5], padding=5),
    widget.TextBox(
        text="",
        background=colors[5],
        foreground=colors[4],
        padding=0,
        fontsize=37,
    ),
    # widget.CurrentLayoutIcon(
    #         custom_icon_paths = [
    # os.path.expanduser("~/.config/qtile/icons")
    # ],
    #         foreground = colors[0],
    #         background = colors[4],
    #         padding = 0,
    #         scale = 0.7
    #         ),
    widget.CurrentLayout(
        foreground=colors[2], background=colors[4], padding=5
    ),
    widget.TextBox(
        text="",
        background=colors[4],
        foreground=colors[5],
        padding=0,
        fontsize=37,
    ),
    widget.Clock(
        foreground=colors[2],
        background=colors[5],
        format="%A, %B %d  [ %H:%M ]",
    ),
    widget.Sep(
        linewidth=0, padding=10, foreground=colors[0], background=colors[5]
    ),
    # widget.Systray(
    #         background = colors[0],
    #         padding = 5
    #         ),
]

screens = [Screen(top=bar.Bar(widgets=widgets_list opacity=1.0, size=20))]

"""
keys.extend(
    [
        Key(
            [mod],
            "i",
            lazy.function(
                Qtile.cmd_spawn(None, group_apps[screens[0].group.name][0])
            ),
            desc="Launch first group program",
        ),
        Key(
            [mod],
            "o",
            Qtile.cmd_spawn(None, group_apps[screens[0].group.name][1]),
            desc="Launch second group program",
        ),
    ]
)
"""

mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(
    float_rules=[
        {"wmclass": "confirm"},
        {"wmclass": "dialog"},
        {"wmclass": "download"},
        {"wmclass": "error"},
        {"wmclass": "file_progress"},
        {"wmclass": "notification"},
        {"wmclass": "splash"},
        {"wmclass": "toolbar"},
        {"wmclass": "confirmreset"},  # gitk
        {"wmclass": "makebranch"},  # gitk
        {"wmclass": "maketag"},  # gitk
        {"wname": "branchdialog"},  # gitk
        {"wname": "pinentry"},  # GPG key password entry
        {"wmclass": "ssh-askpass"},  # ssh-askpass
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"


@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser("~")
    subprocess.call([home + "/.config/qtile/autostart.sh"])


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
