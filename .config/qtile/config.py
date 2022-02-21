"""
My custom Qtile Configuration.
"""
import os
import subprocess
import psutil  # type: ignore
import re
from libqtile.config import Key, Screen, Group, Drag, Click, Match, Rule
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook, qtile
import custom

## CONSTANTS
MODKEY = "mod4"
TERMINAL = os.environ["TERMINAL"]
HOME = os.environ["HOME"]
CONFIG = f"{HOME}/.config/qtile"
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

## VARIABLES
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

## WIDGETS
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
sysinfo_widgets: list[list[tuple]] = [
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
                    "Button1": lazy.spawn(f"{TERMINAL} -e btm -b")
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
                    "Button1": lazy.spawn(f"{TERMINAL} -t btm -e btm -b")
                },
            },
        ),
        (
            custom.Memory,
            {
                "mouse_callbacks": {
                    "Button1": lazy.spawn(f"{TERMINAL} -t btm -e btm -b")
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
                    "Button1": lazy.spawn(f"{TERMINAL} -t btop -e btop")
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
                    "Button1": lazy.spawn(f"{TERMINAL} -t btop -e btop")
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
                    "Button1": lazy.spawn(f"{TERMINAL} -t btop -e btop")
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

## SCREENS
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

## LAYOUTS
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
        Match(wm_class="flameshot"),
        Match(wm_class="yad"),
        Match(wm_class="qalculate-gtk"),
    ],
    **layout_theme | {"border_focus": COLORS["hl2"]},
)

## GROUPS
group_names: list[tuple[str, dict]] = [
    (
        "CHAT",
        {
            "layout": "max",
            "spawn": [
                "nice -n2 discord-canary",
                "nice -n2 discord",
                "nice -n2 signal-desktop-beta",
                "nice -n2 element-desktop",
            ],
            "matches": [
                Match(wm_class="discord"),
                Match(wm_class="signal beta"),
                Match(wm_class="signal"),
                Match(wm_class="element"),
            ],
        },
    ),
    ("AGND", {"layout": "monadtall"}),
    (
        "CLAS",
        {
            "layout": "monadtall",
            "matches": [
                Match(wm_class="zoom"),
                Match(wm_class="microsoft teams - preview"),
            ],
        },
    ),
    (
        "SCHL",
        {"layout": "monadtall", "matches": [Match(wm_class="DesktopEditors")]},
    ),
    ("PRGM", {"layout": "monadtall"}),
    (
        "INET",
        {
            "layout": "monadtall",
            "spawn": ["nice -n3 qbittorrent", "nice -n2 lbry"],
            "matches": [
                Match(wm_class="qbittorrent"),
                Match(wm_class="lbry"),
                Match(wm_class="mailspring"),
            ],
        },
    ),
    (
        "GAME",
        {
            "layout": "max",
            "matches": [
                Match(wm_class="lutris"),
                Match(wm_class="Steam"),
                Match(wm_class="heroic"),
            ],
        },
    ),
    (
        "MUSC",
        {
            "layout": "monadtall",
            "matches": [
                Match(wm_class="nuclear"),
                Match(wm_class="deadbeef"),
            ],
        },
    ),
    (
        "STAT",
        {
            "layout": "max",
            "spawn": [
                "nice -n5 /usr/bin/alacritty -o ipc_socket=false --class btop-spawn -t btop -e btop"
            ],
            "matches": [
                Match(wm_class="btop-spawn"),
            ],
        },
    ),
    (
        "SLAD",
        {
            "layout": "monadtall",
            "spawn": [],
            "matches": [
                Match(wm_class="salad"),
                Match(wm_class="radeon-profile"),
            ],
        },
    ),
]
if os.environ["REAL_GPU"] == "amd":
    # SLAD group
    group_names[-1][1]["spawn"].extend(["nice -n1 salad", "radeon-profile"])
groups = [
    Group(name, label=f"{name}", **kwargs) for name, kwargs in group_names
]


## KEYBINDS
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


@lazy.function
def unfullscreen_all(qtile):
    for i in qtile.current_group.windows:
        i.fullscreen = False


@lazy.function
def toggle_floating(qtile):
    win = qtile.current_window
    if win.fullscreen:
        win.fullscreen = False
        win.floating = True
    else:
        win.floating = not win.floating

@lazy.function
def unfloat_all(qtile):
    for i in qtile.current_group.windows:
        if i.fullscreen:
            continue
        i.floating = False


@lazy.function
def unminimize_all(qtile):
    for i in qtile.current_group.windows:
        i.minimized = False


keys = [
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
        [MODKEY],
        "slash",
        lazy.layout.reset(),
        desc="Normalize window size ratios"
    ),
    Key(
        [MODKEY],
        "equal",
        lazy.layout.increase_nmaster(),
        desc="Increase number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "minus",
        lazy.layout.decrease_nmaster(),
        desc="Decrease number in master pane (Tile)",
    ),
    Key(
        [MODKEY],
        "b",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on current active window",
    ),
    Key(
        [MODKEY, "shift"],
        "b",
        unfullscreen_all,
        desc="Unfullscreen all windows in current group",
    ),
    Key(
        [MODKEY],
        "n",
        toggle_floating,
        desc="Toggle floating on current active window",
    ),
    Key(
        [MODKEY, "shift"],
        "n",
        unfloat_all,
        desc="Unfloat all windows in current group",
    ),
    Key(
        [MODKEY],
        "m",
        lazy.window.toggle_minimize(),
        desc="Toggle minimize on current active window",
    ),
    Key(
        [MODKEY, "shift"],
        "m",
        unminimize_all,
        desc="Unminimize all windows in current group",
    ),
]

# Group keybinds
for i, (name, kwargs) in enumerate(group_names, start=0):
    if i > 10:
        break
    key = "grave" if i == 0 else "0" if i == 10 else str(i)
    keys.extend(
        [
            Key(
                [MODKEY],
                key,
                lazy.group[name].toscreen(),
                desc=f"Switch to group {name}",
            ),
            Key(
                [MODKEY, "shift"],
                key,
                lazy.window.togroup(name, switch_group=True),
                desc=f"Switch to and move focused window to group {name}",
            ),
            Key(
                [MODKEY, "control"],
                key,
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


## HOOKS
@hook.subscribe.startup_once
def on_first_startup():
    """
    Begins startup processes like daemons and the compositor. See
    `./scripts/autostart.sh`.
    """
    subprocess.call([f"{CONFIG}/scripts/autostart.sh"])

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


@hook.subscribe.client_new
def _kde_connect(win):
    """
    Prevents KDE Connect Daemon pointer from creating ghost window
    """
    if win.name == "KDE Connect Daemon":
        s = qtile.current_screen
        win.cmd_static(qtile.screens.index(s), s.x, s.y, s.width, s.height)
