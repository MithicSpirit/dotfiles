"""
My custom Qtile Configuration.
"""
import os
import subprocess
import psutil
from libqtile.config import Key, Screen, Group, Drag, Click, Match
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook, qtile
import custom

# Custom constants
MODKEY = "mod4"
TERMINAL = "alacritty"
HOME = os.environ["HOME"]
CONFIG = f"{HOME}/.config/qtile"
BROWSER = "brave-nightly"
VISUAL = "visual"
colors = {
    "bg": "#292d3e",
    "fg": "#eeffff",
    "hlfg": "#c792ea",
    "hlbg": "#4e5579",
    "hl1": "#bb80b3",
    "hl2": "#7986e7",
}

# Qtile config variables
auto_fullscreen = True
bring_front_click = "floating_only"
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
    "background": colors["bg"],
    "foreground": colors["fg"],
}
extension_defaults = widget_defaults
wmname = "LG3D"


@hook.subscribe.startup_once
def on_first_startup():
    """
    Begins startup processes like daemons and the compositor. See
    `./scripts/autostart.sh`.
    """
    subprocess.call([f"{CONFIG}/scripts/autostart.sh"])


# Set up widgets and screens
widgets = [
    # widget.Spacer(3),
    widget.GroupBox(
        fontsize=12,
        foreground=colors["fg"],
        active=colors["fg"],
        inactive=colors["fg"],
        background=colors["bg"],
        this_current_screen_border=colors["hl1"],
        this_screen_border=colors["hl1"],
        highlight_color=colors["hlbg"],
        urgent_text=colors["fg"],
        urgent_border=colors["hl2"],
        highlight_method="line",
        borderwidth=3,
        rounded=False,
        margin_y=3,
        margin_x=0,
    ),
    widget.Spacer(11),
    widget.CurrentLayoutIcon(
        foreground=colors["fg"],
        background=colors["bg"],
        scale=0.6,
        padding=0,
        padding_x=0,
    ),
    widget.CurrentLayout(foreground=colors["fg"], background=colors["bg"]),
    widget.Spacer(11),
    widget.WindowName(
        foreground=colors["hlfg"],
        for_current_screen="True",
        format="{state}{name}",
    ),
    widget.Spacer(),
    widget.Spacer(10),
    widget.Systray(
        background=colors["bg"], padding=12, padding_x=12, padding_y=12
    ),
    widget.Spacer(16),
]

sysinfo_widgets = [
    [
        (
            custom.CheckUpdates,
            {
                "colour_have_updates": colors["fg"],
                "colour_no_updates": colors["fg"],
                "no_update_string": "0",
                "update_interval": 60 * 60,
                "execute": f'{TERMINAL} -e "{CONFIG}/scripts/updateparu.sh"',
                "custom_command": "checkupdates ; paru -Qua ; true",
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
                    "Button1": lambda: qtile.cmd_spawn(
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
                    "Button1": lambda: qtile.cmd_spawn(
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
                    "Button1": lambda: qtile.cmd_spawn(
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
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm -b")
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
                "foreground_alert": colors["fg"],
                "fmt": "{}",
                "metric": True,
                "tag_sensor": "Tdie",
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
                "padding": 0,
                "padding_x": 0,
                "fontsize": 16,
                "mouse_callbacks": {
                    "Button1": lambda: qtile.cmd_spawn(f"{TERMINAL} -e btm")
                },
            },
        ),
        (
            custom.Thermal,
            {
                "foreground_alert": colors["fg"],
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
    COLOR = colors["hl1"] if i % 2 == 0 else colors["hl2"]
    OTHER = colors["hl2"] if i % 2 == 0 else colors["hl1"]
    if i == 0:
        OTHER = colors["bg"]
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
            widget_type(background=COLOR, foreground=colors["fg"], **kwargs)
        )

screens = [
    Screen(
        top=bar.Bar(
            widgets=widgets, opacity=0.86, size=24, margin=[0, 0, 3, 0]
        ),
        bottom=bar.Gap(3),
        left=bar.Gap(3),
        right=bar.Gap(3),
    )
]

# Set up layouts and groups
layout_theme = {
    "border_width": 2,
    "margin": 2,
    "border_focus": colors["hl1"],
    "border_normal": colors["bg"],
    "ratio": 0.5,
}

layouts = [
    layout.MonadTall(
        change_ratio=0.04, min_ratio=0.22, **layout_theme | {"margin": 4}
    ),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2, **layout_theme),
    layout.Tile(shift_windows=True, **layout_theme),
]
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirm"),
        Match(wm_class="dialog"),
        Match(wm_class="download"),
        Match(wm_class="error"),
        Match(wm_class="file_progress"),
        Match(wm_class="notification"),
        Match(wm_class="splash"),
        Match(wm_class="toolbar"),
        Match(wm_class="ssh-askpass"),
        Match(wm_class="optifine-InstallerFrame"),
        Match(wm_class="authy desktop"),
        Match(wm_class="pinentry-gtk-2"),
        Match(wm_class="lxpolkit"),
        Match(wm_class="org.gnome.Characters"),
        Match(wm_class="zoom", title="Settings"),
        Match(title="Picture in picture"),
        Match(title="Steam Guard - Computer Authorization Required"),
        Match(wm_class="redshift-gtk"),
        Match(wm_class="epicgameslauncher.exe"),
        Match(role="GtkFileChooserDialog"),
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(title="branchdialog"),
        Match(title="pinentry"),
        Match(title="zoom_linux_float_video_window"),
        Match(wm_class="zoom", title="zoom"),
        Match(wm_class="flameshot"),
    ],
    **layout_theme | {"border_focus": colors["hl2"]},
)

group_names = [
    (
        "CHAT",
        {
            "layout": "max",
            "spawn": ["discord-canary", "discord-ptb", "signal-desktop-beta"],
        },
    ),
    ("AGND", {"layout": "monadtall"}),
    ("CLAS", {"layout": "monadtall"}),
    ("SCHL", {"layout": "monadtall"}),
    ("PRGM", {"layout": "monadtall"}),
    ("INET", {"layout": "monadtall", "spawn": ["qbittorrent"]}),
    ("GAME", {"layout": "max"}),
    ("MUSC", {"layout": "monadtall"}),
]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

group_apps = {
    "CHAT": ("discord-canary", "discord-ptb", "signal-desktop-beta"),
    "AGND": (VISUAL, f'{VISUAL} -e "(=school-agenda)"', "false"),
    "CLAS": ("zoom", "teams", "false"),
    "SCHL": (f"{VISUAL} {HOME}/documents/school", "libreoffice", "teams"),
    "PRGM": (VISUAL, f"{VISUAL} {HOME}/documents/coding", "false"),
    "INET": (
        f"{BROWSER} --new-window https://odysee.com/$/following",
        f"{BROWSER} --new-window https://www.youtube.com/feed/subscriptions",
        "false",
    ),
    "GAME": ("lutris", "steam", "heroic"),
    "MUSC": (
        f"{BROWSER} --new-window https://music.youtube.com/library/playlists",
        "deadbeef",
        "false",
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
    # Key([MODKEY, "control"], "q", lazy.restart(), desc="Restart Qtile"),
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
                # if i.compare(parent.window):
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
