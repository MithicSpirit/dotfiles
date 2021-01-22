"""
My custom Qtile Configuration.
"""
import os
import subprocess
import psutil
from libqtile.config import Key, Screen, Group, Drag, Click, Match
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
import custom

# Custom constants
MODKEY = "mod4"
TERMINAL = "urxvtc"
HOME = os.environ["HOME"]
CONFIG = f"{HOME}/.config/qtile"
BROWSER = "brave-nightly"
VISUAL = "visual"
ENVIRON_UPDATE = {
    "PATH": f"{HOME}/.local/bin:{HOME}/.emacs.d/bin:{os.environ['PATH']}",
    "QT_QPA_PLATFORMTHEME": "qt5ct",
}
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
    "hlfg": "#c792ea",
    "hlbg": "#4e5579",
    "hl1": "#bb80b3",
    "hl2": "#7986e7",
}

# Qtile config variables
auto_fullscreen = True
bring_front_click = True
cursor_warp = False
dgroups_key_binder = None
dgroups_app_rules = []
focus_on_window_activation = "smart"
follow_mouse_focus = True
widget_defaults = {
    "font": "Iosevka Mithic Medium",
    "fontsize": 13,
    "padding_x": 5,
    "padding_y": 0,
    "padding": 5,
    "margin": 0,
    "background": colors_dict["bg"],
    "foreground": colors_dict["fg"],
}
extension_defaults = widget_defaults
wmname = "LG3D"

os.environ |= ENVIRON_UPDATE  # python3.9+
# for var in ENVIRON_UPDATE:
#     os.environ[var] = ENVIRON_UPDATE[var]


@hook.subscribe.startup_once
def on_first_startup():
    """
    Begins startup processes like daemons and the compositor. See
    `./scripts/autostart.sh`.
    """
    subprocess.call([f"{CONFIG}/scripts/autostart.sh"])


# Set up widgets and screens
widgets = [
    widget.Spacer(3),
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
    widget.Spacer(11),
    widget.CurrentLayoutIcon(
        foreground=colors_dict["fg"],
        background=colors_dict["bg"],
        scale=0.6,
        padding=0,
        padding_x=0,
    ),
    widget.CurrentLayout(
        foreground=colors_dict["fg"], background=colors_dict["bg"]
    ),
    widget.Spacer(11),
    widget.WindowName(
        foreground=colors_dict["hlfg"], for_current_screen="True"
    ),
    widget.Spacer(),
    widget.Spacer(10),
    widget.Systray(
        background=colors_dict["bg"], padding=12, padding_x=12, padding_y=12
    ),
    widget.Spacer(16),
]

sysinfo_widgets = [
    [
        (
            custom.CheckUpdates,
            {
                "colour_have_updates": colors_dict["fg"],
                "colour_no_updates": colors_dict["fg"],
                "no_update_string": "0",
                "update_interval": 60 * 60,
                "execute": f'{TERMINAL} -e "{CONFIG}/scripts/updateyay.sh"',
                "custom_command": "checkupdates+aur",
            },
        )
    ],
    [
        (
            widget.Net,
            {
                "interface": "enp4s0",
                "format": "{down} ↓",
                "use_bits": True,
                "update_interval": 5,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(
                        f"{TERMINAL} -e {CONFIG}/scripts/speedtest.sh"
                    )
                },
            },
        ),
        (
            widget.TextBox,
            {
                "text": "|",
                "padding": 0,
                "padding_x": 0,
                "fontsize": 16,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(
                        f"{TERMINAL} -e {CONFIG}/scripts/speedtest.sh"
                    )
                },
            },
        ),
        (
            widget.Net,
            {
                "interface": "enp4s0",
                "format": "{up} ↑",
                "use_bits": True,
                "update_interval": 5,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(
                        f"{TERMINAL} -e {CONFIG}/scripts/speedtest.sh"
                    )
                },
            },
        ),
    ],
    [
        (
            custom.CPU,
            {
                "format": "{load_percent}%",
                "update_interval": 10,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(f"{TERMINAL} -e btm -b")
                },
            },
        ),
        (
            widget.TextBox,
            {
                "text": "|",
                "padding": 0,
                "padding_x": 0,
                "fontsize": 16,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(f"{TERMINAL} -e btm -b")
                },
            },
        ),
        (
            custom.Memory,
            {
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(f"{TERMINAL} -e btm -b")
                },
                "update_interval": 30,
            },
        ),
    ],
    [
        (
            custom.Thermal,
            {
                "foreground_alert": colors_dict["fg"],
                "fmt": "{}",
                "metric": True,
                "tag_sensor": "Tdie",
                "update_interval": 10,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(f"{TERMINAL} -e btm")
                },
            },
        ),
        (
            widget.TextBox,
            {
                "text": "|",
                "padding": 0,
                "padding_x": 0,
                "fontsize": 16,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(f"{TERMINAL} -e btm")
                },
            },
        ),
        (
            custom.Thermal,
            {
                "foreground_alert": colors_dict["fg"],
                "fmt": "{}",
                "metric": True,
                "tag_sensor": "edge",
                "update_interval": 10,
                "mouse_callbacks": {
                    "Button1": lambda q: q.cmd_spawn(f"{TERMINAL} -e btm")
                },
            },
        ),
    ],
    [
        (
            custom.Volume,
            {
                "fmt": "Vol: {}",
                "device": "pulse",
                "step": 5,
                "update_interval": 0.5,
                "volume_app": "pavucontrol",
            },
        )
    ],
    [
        (
            widget.Clock,
            {"format": "%H:%M:%S", "update_interval": 1},
        ),
        (
            widget.TextBox,
            {"text": "|", "padding": 0, "padding_x": 0, "fontsize": 16},
        ),
        (
            widget.Clock,
            {"format": "%a, %b %d, %Y", "update_interval": 60},
        ),
        (widget.Spacer, {"length": 3}),
    ],
]
for i, widget_group in enumerate(sysinfo_widgets):
    COLOR = colors_dict["hl1"] if i % 2 == 0 else colors_dict["hl2"]
    OTHER = colors_dict["hl2"] if i % 2 == 0 else colors_dict["hl1"]
    if i == 0:
        OTHER = colors_dict["bg"]
    widgets.append(
        widget.TextBox(
            text="",
            background=OTHER,
            foreground=COLOR,
            fontsize=23,
            padding=0,
            padding_x=0,
        ),
    )
    for widget_type, kwargs in widget_group:
        widgets.append(
            widget_type(
                background=COLOR, foreground=colors_dict["fg"], **kwargs
            )
        )

screens = [
    Screen(
        top=bar.Bar(widgets=widgets, opacity=.9, size=24, margin=[0, 0, 3, 0]),
        bottom=bar.Gap(3),
        left=bar.Gap(3),
        right=bar.Gap(3),
    )
]

# Set up layouts and groups
layout_theme = {
    "border_width": 2,
    "margin": 2,
    "border_focus": colors_dict["hl1"],
    "border_normal": colors_dict["bg"],
}

layouts = [
    layout.MonadTall(change_size=15, **layout_theme | {"margin": 4}),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2, **layout_theme),
    layout.Tile(shift_windows=True, **layout_theme),
]
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
        {"wmclass": "ssh-askpass"},
        {"wmclass": "optifine-InstallerFrame"},
        {"wmclass": "authy desktop"},
        {"wmclass": "pinentry-gtk-2"},
        {"wmclass": "lxpolkit"},
        {"wmclass": "org.gnome.Characters"},
        # {"wmclass": "zoom", "wname": "Settings"},
        {"wname": "Picture in picture"},
        {"wname": "Steam Guard - Computer Authorization Required"},
        {"wmclass": "redshift-gtk"},
        {"wmclass": "epicgameslauncher.exe"},
    ],
    **layout_theme | {"border_focus": colors_dict["hl2"]},
)

group_names = [
    (
        "CHAT",
        {
            "layout": "max",
            "spawn": ["discord-canary", "signal-desktop"],
        },
    ),
    ("AGND", {"layout": "max"}),
    ("CLAS", {"layout": "max"}),
    ("SCHL", {"layout": "monadtall"}),
    ("PRGM", {"layout": "monadtall"}),
    ("INET", {"layout": "monadtall", "spawn": ["qbittorrent"]}),
    ("GAME", {"layout": "max", "spawn": ["steam -silent"]}),
    ("MUSC", {"layout": "monadtall"}),
]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

group_apps = {
    "CHAT": ("discord-canary", "element-desktop"),
    "AGND": (VISUAL, f'{VISUAL} -e "(school-agenda)"'),
    "CLAS": ("zoom", "teams"),
    "SCHL": (f"{VISUAL} {HOME}/documents/school", "libreoffice"),
    "PRGM": (VISUAL, f"{VISUAL} {HOME}/documents/coding"),
    "INET": (
        f"{BROWSER} --new-window https://odysee.com/$/following",
        f"{BROWSER} --new-window https://www.youtube.com/feed/subscriptions",
    ),
    "GAME": ("lutris", "steam"),
    "MUSC": (
        f"{BROWSER} --new-window https://music.youtube.com/library/playlists",
        "deadbeef",
    ),
}


# Set up Keybinds
@lazy.function
def custom_app_1(qtile):
    curr_group = qtile.current_group.name
    prog = group_apps[curr_group][0]
    qtile.cmd_spawn(prog)


@lazy.function
def custom_app_2(qtile):
    curr_group = qtile.current_group.name
    prog = group_apps[curr_group][1]
    qtile.cmd_spawn(prog)


@lazy.function
def layout_monadtall(qtile):
    qtile.current_group.use_layout(0)


@lazy.function
def layout_max(qtile):
    qtile.current_group.use_layout(1)


@lazy.function
def layout_stack(qtile):
    qtile.current_group.use_layout(2)


@lazy.function
def layout_tile(qtile):
    qtile.current_group.use_layout(3)


@lazy.function
def layout_floating(qtile):
    qtile.current_group.use_layout(4)


keys = [
    # Launching programs
    Key(
        [MODKEY],
        "Return",
        lazy.spawn(f"{TERMINAL} -e /usr/bin/fish"),
        desc="Launch terminal",
    ),
    Key([MODKEY], "a", lazy.spawn(BROWSER), desc="Launch browser"),
    Key(
        [MODKEY, "shift"],
        "a",
        lazy.spawn("thunar"),
        desc="Launch file manager",
    ),
    Key(
        [MODKEY, "shift"],
        "Return",
        lazy.spawn('dmenu_run -p "Run"'),
        desc="Open dmenu run launcher",
    ),
    Key(
        [MODKEY],
        "i",
        custom_app_1,
        desc="Launch the 1st custom program for the current group",
    ),
    Key(
        [MODKEY, "shift"],
        "i",
        custom_app_2,
        desc="Launch the 2nd custom program for the current group",
    ),
    # Misc
    Key(
        [],
        "XF86AudioPlay",
        lazy.spawn("playerctl play-pause"),
        desc="Toggle pause",
    ),
    Key(
        [MODKEY],
        "v",
        lazy.spawn("greenclip-dmenu"),
        desc="Open clipboard selection menu",
    ),
    # Key(
    #     [MODKEY],
    #     "space",
    #     keyboard_switch_layout,
    #     desc="Switch keyboard layout",
    # ),
    # Layouts
    Key([MODKEY], "Tab", layout_stack, desc="Set layout to stack"),
    Key([MODKEY, "shift"], "Tab", layout_max, desc="Set layout to max"),
    Key(
        [MODKEY, "control"],
        "Tab",
        layout_monadtall,
        desc="Set layout to monadtall",
    ),
    Key(
        [MODKEY, "shift", "control"],
        "Tab",
        layout_tile,
        desc="Set layout to tile",
    ),
    # Session control
    Key(
        [MODKEY, "shift"],
        "q",
        lazy.spawn(f"{HOME}/.local/bin/betterlockscreen -l --off 15"),
        desc="Lock screen",
    ),
    Key([MODKEY, "control"], "q", lazy.restart(), desc="Restart Qtile"),
    Key(
        [MODKEY, "control", "shift"],
        "q",
        lazy.shutdown(),
        desc="Close Qtile",
    ),
    # Window movement and management
    Key(
        [MODKEY],
        "j",
        lazy.layout.down(),
        desc="Move focus up in current stack pane",
    ),
    Key(
        [MODKEY],
        "k",
        lazy.layout.up(),
        desc="Move focus down in current stack pane",
    ),
    Key(
        [MODKEY, "shift"],
        "j",
        lazy.layout.shuffle_down(),
        desc="Move window down in current stack",
    ),
    Key(
        [MODKEY, "shift"],
        "k",
        lazy.layout.shuffle_up(),
        desc="Move window up in current stack",
    ),
    Key(
        [MODKEY],
        "h",
        lazy.layout.previous(),
        desc="Move focus to previous stack",
    ),
    Key([MODKEY], "l", lazy.layout.next(), desc="Move focus to next stack"),
    Key(
        [MODKEY, "shift"],
        "h",
        lazy.layout.client_to_previous(),
        desc="Move window to previous stack",
    ),
    Key(
        [MODKEY, "shift"],
        "l",
        lazy.layout.client_to_next(),
        desc="Move window to next stack",
    ),
    Key(
        [MODKEY, "shift"], "x", lazy.window.kill(), desc="Close active window"
    ),
    Key(
        [MODKEY],
        "period",
        lazy.layout.grow(),
        desc="Expand window (MonadTall), "
        + "increase number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "comma",
        lazy.layout.shrink(),
        desc="Shrink window (MonadTall), "
        + "decrease number in master pane (Tile)",
    ),
    Key(
        [MODKEY], "m", lazy.layout.reset(), desc="Normalize window size ratios"
    ),
    Key(
        [MODKEY],
        "equal",
        lazy.layout.increase_nmaster(),
        desc="Expand window (MonadTall), "
        + "increase number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "minus",
        lazy.layout.decrease_nmaster(),
        desc="Shrink window (MonadTall), "
        + "decrease number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "n",
        lazy.layout.maximize(),
        desc="toggle window between minimum and maximum sizes",
    ),
    Key(
        [MODKEY, "shift"],
        "m",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen",
    ),
    Key([MODKEY], "f", lazy.window.toggle_floating(), desc="Toggle floating"),
    # Screenshot
    Key(
        [MODKEY],
        "p",
        lazy.spawn(f"{CONFIG}/scripts/maim-full.sh"),
        desc="Take full screenshot and store in clipboard",
    ),
    Key(
        [MODKEY, "control"],
        "p",
        lazy.spawn(f"{CONFIG}/scripts/maim-window.sh"),
        desc="Take screenshot of the active window and store in clipboard",
    ),
    Key(
        [MODKEY, "shift"],
        "p",
        lazy.spawn(f"{CONFIG}/scripts/maim-region.sh"),
        desc="Take screenshot of interactively selected region and store in "
        + "clipboard",
    ),
    Key(
        [MODKEY, "control", "shift"],
        "p",
        lazy.spawn(f"{CONFIG}/scripts/maim-save.sh"),
        desc="Take full screenshot and store as file",
    ),
]

# Group keybinds
for i, (name, kwargs) in enumerate(group_names, start=0):
    i = "grave" if i == 0 else str(i)
    keys.extend(
        [
            Key(
                [MODKEY],
                i,
                lazy.group[name].toscreen(),
                desc=f"Switch to group {name}",
            ),
            Key(
                [MODKEY, "shift"],
                i,
                lazy.window.togroup(name, switch_group=True),
                desc=f"Switch to and move focused window to group {name}",
            ),
            Key(
                [MODKEY, "control"],
                i,
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

# Window swallowing
SWALLOW_PARENT = {Match(wm_class="URxvt")}
NO_SWALLOW_CHILD = {Match(title="Event Tester")}


@hook.subscribe.client_new
def _swallow(window):
    for i in NO_SWALLOW_CHILD:
        if i.compare(window):
            return
    pid = window.window.get_net_wm_pid()
    ppid = psutil.Process(pid).ppid()
    cpids = {
        c.window.get_net_wm_pid(): wid
        for wid, c in window.qtile.windows_map.items()
    }
    for _ in range(5):
        if not ppid:
            return
        if ppid in cpids:
            parent = window.qtile.windows_map.get(cpids[ppid])
            for i in SWALLOW_PARENT:
                if i.compare(parent.window):
                    window.parent = parent
                    parent.minimized = True
                    return
            return
        ppid = psutil.Process(ppid).ppid()


@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, "parent"):
        window.parent.minimized = False
