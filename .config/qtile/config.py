"""
My custom Qtile Configuration.
"""
import os
import subprocess
import psutil
import re
from libqtile.config import Key, Screen, Group, Drag, Click, Match, Rule
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook, qtile
import custom

# Custom constants
MODKEY = "mod4"
TERMINAL = "alacritty"
HOME = os.environ["HOME"]
CONFIG = f"{HOME}/.config/qtile"
BROWSER = "librewolf"
VISUAL = "visual"
COLORS = {
    ## Nord
    "bg": "#2e3440",
    "fg": "#eceff4",
    "hlbg": "#434c5e",
    "hl1": "#88c0d0",
    "hl2": "#81a1c1",
    "hlfg": "#2e3440",
    ## Palenight
    # "bg": "#292d3e",
    # "fg": "#eeffff",
    # "hlfg": "#c792ea", # replaced with dark text
    # "hlbg": "#4e5579",
    # "hl1": "#bb80b3",
    # "hl2": "#7986e7",
}

# Qtile config variables
auto_fullscreen = True
bring_front_click = "floating_only"
cursor_warp = False
dgroups_key_binder = None
dgroups_app_rules: list[Rule] = []
focus_on_window_activation = "urgent"
follow_mouse_focus = True
widget_defaults = {
    "font": "Iosevka Mithic Semibold",
    "fontsize": 14,
    "padding_x": 5,
    "padding_y": 0,
    "padding": 5,
    "margin": 0,
    "background": COLORS["bg"],
    "foreground": COLORS["fg"],
    "markup": True,
}
extension_defaults = widget_defaults
wmname = "Qtile"


@hook.subscribe.startup_once
def on_first_startup():
    """
    Begins startup processes like daemons and the compositor. See
    `./scripts/autostart.sh`.
    """
    subprocess.call([f"{CONFIG}/scripts/autostart.sh"])


# Set up widgets and screens
widgets = [
    widget.GroupBox(
        fontsize=12,
        font="Iosevka Mithic Extrabold",
        foreground=COLORS["fg"],
        active=COLORS["fg"],
        inactive=COLORS["fg"],
        background=COLORS["bg"],
        this_current_screen_border=COLORS["hl1"],
        this_screen_border=COLORS["hl1"],
        highlight_color=COLORS["hlbg"],
        urgent_text=COLORS["fg"],
        urgent_border=COLORS["hl2"],
        highlight_method="line",
        borderwidth=3,
        rounded=False,
        margin_y=3,
        margin_x=0,
        disable_drag=True,
        use_mouse_wheel=False,
    ),
    widget.Spacer(11),
    widget.CurrentLayout(
        foreground=COLORS["hl2"],
        background=COLORS["bg"],
        font="Iosevka Mithic Medium",
    ),
    widget.Spacer(11),
    widget.WindowName(
        foreground=COLORS["hl1"],
        for_current_screen="True",
        font="Overpass",
        fontsize=15,
        fmt="<b>{}</b>",
        format="{state}{name}",
        max_chars=0,
        width=bar.STRETCH,
    ),
    widget.Spacer(10),
    widget.Systray(
        background=COLORS["bg"], padding=12, padding_x=12, padding_y=12
    ),
    widget.Spacer(16),
]

PIPE_SEPARATOR_PADDING = -2
sysinfo_widgets = [
    [
        (
            custom.CheckUpdates,
            {
                "colour_have_updates": COLORS["fg"],
                "colour_no_updates": COLORS["fg"],
                "no_update_string": "0",
                "update_interval": 60 * 60,
                "execute": f'{TERMINAL} -e "{CONFIG}/scripts/updateparu.sh"',
                "custom_command": "checkupdates ; paru -Qua | grep -v '\[ignored\]' ; true",
            },
        )
    ],
    [
        (
            custom.CPU,
            {
                "format": "{load_percent}%",
                "update_interval": 10,
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm -b")
                },
            },
        ),
        (
            widget.TextBox,
            {
                "text": "|",
                "padding": PIPE_SEPARATOR_PADDING,
                "fontsize": 18,
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm -b")
                },
            },
        ),
        (
            custom.Memory,
            {
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm -b")
                },
                "update_interval": 30,
            },
        ),
    ],
    [
        (
            custom.Thermal,
            {
                "foreground_alert": COLORS["hlfg"],
                "fmt": "{}",
                "metric": True,
                "tag_sensor": "Tctl",
                "update_interval": 10,
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm")
                },
            },
        ),
        (
            widget.TextBox,
            {
                "text": "|",
                "padding": PIPE_SEPARATOR_PADDING,
                "fontsize": 18,
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm")
                },
            },
        ),
        (
            custom.Thermal,
            {
                "foreground_alert": COLORS["hlfg"],
                "fmt": "{}",
                "metric": True,
                "tag_sensor": "edge",
                "update_interval": 10,
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm")
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
            {"text": "|", "padding": PIPE_SEPARATOR_PADDING, "fontsize": 18},
        ),
        (
            widget.Clock,
            {"format": "%a, %b %d, %Y", "update_interval": 1},
        ),
        (widget.Spacer, {"length": 3}),
    ],
]
if os.path.exists("/sys/class/power_supply/BAT1"):
    sysinfo_widgets.insert(
        -1,
        [
            (
                widget.Battery,
                {
                    "charge_char": "↑",
                    "discharge_char": "↓",
                    "empty_char": "!",
                    "full_char": "¡",
                    "unknown_char": "?",
                    "battery": "BAT1",
                    "format": "Bat: {percent:2.0%} {char}",
                    "low_percentage": -1,
                    "update_interval": 20,
                    "notify_below": 0.35,
                    "show_short_text": False,
                },
            ),
        ],
    )
for i, widget_group in enumerate(sysinfo_widgets):
    COLOR = COLORS["hl1"] if i % 2 == 0 else COLORS["hl2"]
    OTHER = COLORS["hl2"] if i % 2 == 0 else COLORS["hl1"]
    if i == 0:
        OTHER = COLORS["bg"]
    widgets.append(
        widget.TextBox(
            text="",
            background=OTHER,
            foreground=COLOR,
            fontsize=23,
            padding=0,
        ),
    )
    for widget_type, kwargs in widget_group:
        widgets.append(
            widget_type(background=COLOR, foreground=COLORS["hlfg"], **kwargs)
        )

screens = [
    Screen(
        top=bar.Bar(
            widgets=widgets,
            opacity=0.86,
            size=24,
            margin=[0, 0, 3, 0],
        ),
        bottom=bar.Gap(3),
        left=bar.Gap(3),
        right=bar.Gap(3),
    )
]

# Set up layouts and groups
layout_theme = {
    "border_width": 3,
    "margin": 2,
    "border_focus": COLORS["hl1"],
    "border_normal": COLORS["bg"],
    "ratio": 0.5,
}

layouts = [
    layout.MonadTall(
        change_ratio=0.04,
        min_ratio=0.26,
        max_ratio=0.74,
        **layout_theme | {"margin": 4},
    ),
    layout.Max(**layout_theme | {"margin": 0}),
    layout.Stack(num_stacks=2, **layout_theme),
    layout.Tile(shift_windows=True, **layout_theme),
]
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="ssh-askpass"),
        Match(wm_class="optifine-InstallerFrame"),
        Match(wm_class="authy desktop"),
        Match(wm_class="pinentry-gtk-2"),
        Match(wm_class="lxpolkit"),
        Match(wm_class="org.gnome.Characters"),
        Match(wm_class="zoom", title="Settings"),
        Match(title="Picture in picture"),
        Match(wm_class="redshift-gtk"),
        Match(role="GtkFileChooserDialog"),
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(title="branchdialog"),
        Match(title="pinentry"),
        Match(title="zoom_linux_float_video_window"),
        Match(
            func=lambda client: ("zoom" == client.name)
            and ("zoom" == client.window.get_wm_class()[0])
        ),
        Match(wm_class="flameshot"),
        Match(wm_class="yad"),
        Match(wm_class="qalculate-gtk"),
    ],
    **layout_theme | {"border_focus": COLORS["hl2"]},
)

group_names = [
    (
        "CHAT",
        {
            "layout": "max",
            "spawn": [
                "nice -n2 discord-canary",
                "nice -n2 discord-ptb",
                "nice -n2 signal-desktop-beta",
            ],
        },
    ),
    ("AGND", {"layout": "monadtall"}),
    ("CLAS", {"layout": "monadtall"}),
    ("SCHL", {"layout": "monadtall"}),
    ("PRGM", {"layout": "monadtall"}),
    (
        "INET",
        {
            "layout": "monadtall",
            "spawn": ["nice -n3 qbittorrent", "nice -n2 lbry"],
        },
    ),
    ("GAME", {"layout": "max"}),
    ("MUSC", {"layout": "monadtall"}),
    ("SLAD", {"layout": "monadtall", "spawn": []}),
]
if os.environ["REAL_GPU"] == "amd":
    # SLAD group
    group_names[-1][1]["spawn"].extend(["nice -n1 salad", "radeon-profile"])
groups = [
    Group(name, label=f"{name}", **kwargs) for name, kwargs in group_names
]

group_apps = {
    "CHAT": (
        "nice -n2 discord-canary",
        "nice -n2 discord-ptb",
        "nice -n2 signal-desktop-beta",
        "nice -n2 element-desktop",
    ),
    "AGND": (
        VISUAL,
        f'{VISUAL} -e "(=school-agenda)"',
        f'{VISUAL} -e "(=mu4e)"',
        "false",
    ),
    "CLAS": ("nice -n5 zoom", "nice -n5 teams", "false"),
    "SCHL": (
        f"{VISUAL} {HOME}/documents/school",
        "libreoffice",
        "nice -n5 teams",
        "false",
    ),
    "PRGM": (VISUAL, f"{VISUAL} {HOME}/documents/coding", "false", "false"),
    "INET": (
        "nice -n2 lbry",
        f"{BROWSER} --new-window https://www.youtube.com/feed/subscriptions",
        "nice -n5 mailspring",
        f"{BROWSER} --new-window https://odysee.com/$/following",
    ),
    "GAME": ("lutris", "steam", "heroic", "false"),
    "MUSC": (
        f"{BROWSER} --new-window https://music.youtube.com/library/playlists",
        "deadbeef",
        "audiotube",
        "false",
    ),
    "SLAD": (
        "nice -n1 salad",
        "radeon-profile",
        f"nice -n20 {TERMINAL} -e sh",
        "false",
    ),
}


# Set up Keybinds
def custom_app(num):
    @lazy.function
    def launch_app(qtile):
        curr_group = qtile.current_group.name
        prog = group_apps[curr_group][num - 1]
        qtile.cmd_spawn(prog)

    return launch_app


def layout_change(layout):
    layouts = {
        "monadtall": 0,
        "max": 1,
        "stack": 2,
        "tile": 3,
    }
    layout_num = layouts[layout]
    return lazy.function(lambda q: q.current_group.use_layout(layout_num))


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


keys = [
    # Launching programs
    Key(
        [MODKEY],
        "i",
        custom_app(1),
        desc="Launch the 1st custom program for the current group",
    ),
    Key(
        [MODKEY, "shift"],
        "i",
        custom_app(2),
        desc="Launch the 2nd custom program for the current group",
    ),
    Key(
        [MODKEY, "control"],
        "i",
        custom_app(3),
        desc="Launch the 3rd custom program for the current group",
    ),
    Key(
        [MODKEY, "control", "shift"],
        "i",
        custom_app(4),
        desc="Launch the 4th custom program for the current group",
    ),
    # Misc
    Key(
        [MODKEY],
        "v",
        lazy.spawn("dmenu-greenclip"),
        desc="Open clipboard selection menu",
    ),
    Key(
        [MODKEY, "shift"],
        "space",
        lazy.spawn("dmenu-xkb"),
        desc="Open keyboard layout selection menu",
    ),
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
        lazy.spawn("/usr/bin/betterlockscreen -l -- --ignore-empty-password"),
        desc="Lock screen",
    ),
    # Key([MODKEY, "control"], "q", lazy.restart(), desc="Restart Qtile"),
    Key(
        [MODKEY, "control", "shift"],
        "q",
        lazy.spawn("dmenu-shutdown"),
        desc="Spawns dmenu script for logging off or shutting down",
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
        [MODKEY, "control"],
        "x",
        lazy.spawn("xkill"),
        desc="Launch xkill to close a program",
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
]

# Group keybinds
for i, (name, kwargs) in enumerate(group_names, start=0):
    if i > 10:
        break
    i = "grave" if i == 0 else "0" if i == 10 else str(i)
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
SWALLOW_PARENT = {
    Match(wm_class="urxvt"),
    Match(wm_class="Alacritty"),
}
NO_SWALLOW_CHILD = {
    Match(title="Event Tester"),
    Match(wm_class="qutebrowser"),
    Match(wm_class="urxvt"),
    Match(wm_class="Alacritty"),
}


@hook.subscribe.client_new
def _swallow(window):
    """
    Minimize parent windows (in `SWALLOW_PARENT`) once a child window
    (not in `NO_SWALLOW_CHILD`) is spawned.
    """
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
                if i.compare(parent):
                    window.parent = parent
                    parent.minimized = True
                    return
        ppid = psutil.Process(ppid).ppid()


@hook.subscribe.client_killed
def _unswallow(window):
    """
    Unminimize parent windows once the child window closes.
    """
    if hasattr(window, "parent"):
        window.parent.minimized = False
