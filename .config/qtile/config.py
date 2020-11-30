"""
My custom Qtile Configuration.
"""
import os
import subprocess
from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.utils import guess_terminal

# Custom constants
MODKEY = "mod4"
TERMINAL = "urxvtc"
HOME = os.path.expanduser("~")
CONFIG = f"{HOME}/.config/qtile"
BROWSER = "brave-nightly"
VISUAL = f"{HOME}/VISUAL"
# colors = [
#     "#292d3e",  # panel background
#     "#4e5579",  # background for current screen tab
#     "#eeffff",  # font color for group names
#     "#bb80b3",  # border line color for current tab
#     "#bb80b3",  # border line color for other tab and odd widgets
#     "#7986e7",  # color for the even widgets
#     "#e1acff",  # window name
# ]
colors_dict = {
    "bg": "#292d3e",
    "fg": "#eeffff",
    "hlbg": "#4e5579",
    "hl1": "#bb80b3",
    "hl2": "#7986e7",
}

# Qtile config variables
auto_fullscreen = True
bring_front_click = True
cursor_warp = False
follow_mouse_focus = True
dgroups_key_binder = None
dgroups_app_rules = []
widget_defaults = {
    "font": "Iosevka Mithic Medium",
    "fontsize": 13,
    "padding_x": 4,
    "padding_y": 0,
    "margin": 0,
    "background": colors_dict["bg"],
    "foreground": colors_dict["fg"],
}


@hook.subscribe.startup_once
def on_first_startup():
    """
    Begins startup processes like daemons and the compositor. See
    `./autostart.sh`.
    """
    subprocess.call([f"{CONFIG}/autostart.sh"])


# Set up widgets and screens
widgets = [
    widget.GroupBox(
        fontsize=12,
        foreground=colors_dict["fg"],
        active=colors_dict["fg"],
        inactive=colors_dict["fg"],
        background=colors_dict["bg"],
        this_current_screen_border=colors_dict["hl1"],
        this_screen_border=colors_dict["hl1"],
        highlight_color=colors_dict["hlbg"],
        urgent_text=colors_dict["fg"],
        urgent_border=colors_dict["hl2"],
        highlight_method="line",
        borderwidth=3,
        rounded=False,
        margin_y=3,
        margin_x=0,
    ),
    widget.Spacer(20),
    widget.CurrentLayoutIcon(
        foreground=colors_dict["fg"], background=colors_dict["bg"], scale=0.6, padding=0
    ),
    widget.CurrentLayout(foreground=colors_dict["fg"], background=colors_dict["bg"]),
    widget.Spacer(),
    widget.Systray(background=colors_dict["bg"], padding=14),
    widget.Spacer(20),
]

sysinfo_widgets = [
    [
        (
            widget.CheckUpdates,
            {
                "colour_have_updates": colors_dict["fg"],
                "colour_no_updates": colors_dict["fg"],
                "fmt": "{} Updates",
                "no_update_string": "0 Updates",
                "update_interval": 60,
                # "distro": "Arch_yay",
                "execute": f"{TERMINAL} -e sudo pacman -Syu",
                "custom_command": "checkupdates+aur",
            },
        )
    ],
    [
        (widget.CPU, {"format": "{load_percent}%"}),
        (widget.TextBox, {"text": "|", "padding": 0}),
        (
            widget.ThermalSensor,
            {
                "foreground_alert": colors_dict["fg"],
                "fmt": "{}",
                "metric": True,
                "tag_sensor": "Tdie",
            },
        ),
        (widget.TextBox, {"text": "|", "padding": 0}),
        (widget.Memory, {"format": "{MemUsed} MB"}),
    ],
    [
        (
            widget.Net,
            {"interface": "enp4s0", "format": "{down} ↓ {up} ↑", "use_bits": True},
        ),
    ],
    [(widget.Volume, {"fmt": "Vol: {}", "device": "pulse", "step": "5"})],
    [(widget.Clock, {"format": "%H:%M:%S | %a, %b %d"})],
]
for i, widget_group in enumerate(sysinfo_widgets):
    COLOR = colors_dict["hl1"] if i % 2 == 0 else colors_dict["hl2"]
    OTHER = colors_dict["hl2"] if i % 2 == 0 else colors_dict["hl1"]
    if i == 0:
        OTHER = colors_dict["bg"]
    widgets.append(
        widget.TextBox(
            text="", background=OTHER, foreground=COLOR, padding=0, fontsize=23
        ),
    )
    for widget_type, kwargs in widget_group:
        widgets.append(
            widget_type(background=COLOR, foreground=colors_dict["fg"], **kwargs)
        )


screens = [Screen(top=bar.Bar(widgets=widgets, opacity=0.9, size=24))]

# Set up layouts and groups
layout_theme = {
    "border_width": 3,
    "margin": 5,
    "border_focus": colors_dict["hl1"],
    "border_normal": colors_dict["bg"],
}

layouts = [
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2, **layout_theme),
    layout.Tile(shift_windows=True, **layout_theme),
    layout.Floating(
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
            {"wmclass": "ssh-askpass"},
            {"wmclass": "optifine-InstallerFrame"},
        ],
        border_focus=colors_dict["hl2"],
        **{key: layout_theme[key] for key in layout_theme if key != "border_focus"},
    ),
]

group_names = [
    (
        "CHAT",
        {"layout": "max", "spawn": ["discord-canary", "element-desktop-nightly"]},
    ),
    ("AGND", {"layout": "max"}),
    ("CLAS", {"layout": "max"}),
    ("SCHL", {"layout": "stack"}),
    ("PRGM", {"layout": "monadtall"}),
    ("INET", {"layout": "max"}),
    ("MUSC", {"layout": "max", "spawn": ["deadbeef"]}),
]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

group_apps = {
    "CHAT": ("discord-canary", "element-desktop-nightly"),
    "AGND": (f'{VISUAL} -e "(school-agenda)"',) * 2,
    "CLAS": ("zoom", "teams"),
    "SCHL": (f"{VISUAL} {HOME}/documents/school", "libreoffice"),
    "PRGM": (f"{VISUAL} {HOME}/documents/coding",) * 2,
    "INET": (
        f"{BROWSER} --new-window https://www.youtube.com/feed/subscriptions",
        f"{BROWSER} --new-window https://mail.google.com/mail/u/0/",
    ),
    "MUSC": (
        "deadbeef",
        f"{BROWSER} --new-window https://music.youtube.com/library/playlists",
    ),
}


# Set up Keybinds
@lazy.function
def custom_app_1(qtile):
    """
    Open the 1st program in `group_apps` based on the current group.
    """
    curr_group = qtile.current_group.name
    prog = group_apps[curr_group][0]
    qtile.cmd_spawn(prog)


@lazy.function
def custom_app_2(qtile):
    """
    Open the 2st program in `group_apps` based on the current group.
    """
    curr_group = qtile.current_group.name
    prog = group_apps[curr_group][1]
    qtile.cmd_spawn(prog)


@lazy.function
def layout_monadtall(qtile):
    """
    Set current layout to `monadtall`.
    """
    qtile.current_group.use_layout(0)


@lazy.function
def layout_max(qtile):
    """
    Set current layout to `max`.
    """
    qtile.current_group.use_layout(1)


@lazy.function
def layout_stack(qtile):
    """
    Set current layout to `stack`.
    """
    qtile.current_group.use_layout(2)


@lazy.function
def layout_tile(qtile):
    """
    Set current layout to `tile`.
    """
    qtile.current_group.use_layout(3)


@lazy.function
def layout_floating(qtile):
    """
    Set current layout to `floating`.
    """
    qtile.current_group.use_layout(4)


keys = [
    # Launching programs
    Key([MODKEY], "Return", lazy.spawn(TERMINAL), desc="Launch terminal"),
    Key([MODKEY], "a", lazy.spawn(BROWSER), desc="Launch browser window"),
    Key(
        [MODKEY, "shift"], "a", lazy.spawn(VISUAL), desc="Launch graphical text editor"
    ),
    Key(
        [MODKEY, "shift"],
        "Return",
        lazy.spawn('dmenu_run -p "Run"'),
        desc="Dmenu Run Launcher",
    ),
    Key(
        [MODKEY],
        "i",
        custom_app_1,
        desc="Open the 1st custom program for the current group",
    ),
    Key(
        [MODKEY, "shift"],
        "i",
        custom_app_2,
        desc="Open the 2nd custom program for the current group",
    ),
    # Misc
    Key([], "XF86AudioPlay", lazy.spawn("playerctl play-pause"), desc="Toggle pause"),
    Key([MODKEY], "v", lazy.spawn("clipcat-menu"), desc="Open clipcat dmenu"),
    # Layouts
    Key([MODKEY], "Tab", layout_stack, desc="Set layout to stack"),
    Key([MODKEY, "shift"], "Tab", layout_max, desc="Set layout to max"),
    Key([MODKEY, "control"], "Tab", layout_monadtall, desc="Set layout to monadtall"),
    Key([MODKEY, "shift", "control"], "Tab", layout_tile, desc="Set layout to tile"),
    Key([MODKEY, "shift"], "f", layout_floating, desc="Set layout to floating"),
    # Session control
    Key([MODKEY, "shift"], "q", lazy.spawn(f"{HOME}/.local/bin/betterlockscreen -l --off 15"), desc="Lock screen"),
    Key([MODKEY, "control"], "q", lazy.restart(), desc="Restart Qtile"),
    Key([MODKEY, "control", "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    # Window movement and management
    Key([MODKEY], "j", lazy.layout.down(), desc="Move focus up in current stack pane"),
    Key([MODKEY], "k", lazy.layout.up(), desc="Move focus down in current stack pane"),
    Key(
        [MODKEY, "shift"],
        "j",
        lazy.layout.shuffle_down(),
        desc="Move windows down in current stack",
    ),
    Key(
        [MODKEY, "shift"],
        "k",
        lazy.layout.shuffle_up(),
        desc="Move windows up in current stack",
    ),
    Key([MODKEY], "h", lazy.layout.previous(), desc="Move focus to next stack"),
    Key([MODKEY], "l", lazy.layout.next(), desc="Move focus to next stack"),
    Key(
        [MODKEY, "shift"],
        "h",
        lazy.layout.client_to_previous(),
        desc="Move windows down in current stack",
    ),
    Key(
        [MODKEY, "shift"],
        "l",
        lazy.layout.client_to_next(),
        desc="Move windows up in current stack",
    ),
    Key([MODKEY, "shift"], "x", lazy.window.kill(), desc="Kill active window"),
    Key(
        [MODKEY],
        "period",
        lazy.layout.grow(),
        desc="Expand window (MonadTall), increase number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "comma",
        lazy.layout.shrink(),
        desc="Shrink window (MonadTall), decrease number in master pane (Tile)",
    ),
    Key([MODKEY], "m", lazy.layout.reset(), desc="normalize window size ratios"),
    Key(
        [MODKEY],
        "equal",
        lazy.layout.increase_nmaster(),
        desc="Expand window (MonadTall), increase number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "minus",
        lazy.layout.decrease_nmaster(),
        desc="Shrink window (MonadTall), decrease number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "n",
        lazy.layout.maximize(),
        desc="toggle window between minimum and maximum sizes",
    ),
    Key(
        [MODKEY, "shift"],
        "n",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen",
    ),
    Key([MODKEY], "f", lazy.window.toggle_floating(), desc="toggle floating"),
    # Screenshot
    Key(
        [MODKEY],
        "p",
        lazy.spawn(f"{CONFIG}/maim/full.sh"),
        desc="Take full screenshot and store in clipboard",
    ),
    Key(
        [MODKEY, "control"],
        "p",
        lazy.spawn(f"{CONFIG}/maim/window.sh"),
        desc="Take screenshot of the active window and store in clipboard",
    ),
    Key(
        [MODKEY, "shift"],
        "p",
        lazy.spawn(f"{CONFIG}/maim/region.sh"),
        desc="Take screenshot of interactively selected region and store in clipboard",
    ),
    Key(
        [MODKEY, "control", "shift"],
        "p",
        lazy.spawn(f"{CONFIG}/maim/save.sh"),
        desc="Take full screenshot and store as file",
    ),
]

# Group keybinds
for i, (name, kwargs) in enumerate(group_names, start=1):
    keys.extend(
        [
            Key(
                [MODKEY],
                str(i),
                lazy.group[name].toscreen(),
                desc=f"Switch to group {name}",
            ),
            Key(
                [MODKEY, "shift"],
                str(i),
                lazy.window.togroup(name, switch_group=True),
                desc=f"Switch to & move focused window to group {name}",
            ),
            Key(
                [MODKEY, "control"],
                str(i),
                lazy.window.togroup(name),
                desc=f"Move focused window to group {name}",
            ),
        ]
    )

mouse = [
    Drag(
        [MODKEY],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [MODKEY],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    Click([MODKEY], "Button2", lazy.window.bring_to_front()),
    Click([MODKEY, "shift"], "Button2", lazy.window.bring_to_front()),
]
