"""
Custom widgets for Qtile.
"""
from subprocess import CalledProcessError
import psutil
import subprocess
import threading
from libqtile.widget import base
from libqtile.log_utils import logger

from libqtile.widget.check_updates import CheckUpdates as OldCheckUpdates
from libqtile.widget.cpu import CPU as OldCPU
from libqtile.widget.memory import Memory as OldMemory
from libqtile.widget.volume import Volume as OldVolume
from libqtile.widget.sensors import ThermalSensor as OldThermal


class CheckUpdates(OldCheckUpdates):
    """
    Shows number of pending updates in different package mangers.
    """

    defaults = [
        ("distro", "Arch", "Name of your distribution"),
        (
            "custom_command",
            None,
            "Custom shell command for checking updates "
            + "(counts the lines of the output)",
        ),
        ("update_interval", 60 * 60, "Update interval in seconds."),
        ("execute", None, "Command to execute on click"),
        ("colour_no_updates", "ffffff", "Colour when there's no updates."),
        ("colour_have_updates", "ffffff", "Colour when there are updates."),
        (
            "restart_indicator",
            "",
            "Indicator to represent reboot is required. (Ubuntu only)",
        ),
    ]

    def _check_updates(self):
        """
        Returns string of how many updates are available. Formatted with
        `format_num_updates` unless an error occurs.
        """
        try:
            if self.custom_command is None:
                updates = self.call_process(self.cmd)
            else:
                updates = self.call_process(self.custom_command, shell=True)
                self.subtr = 0
        except CalledProcessError:
            return "  0 Pkg"  # if it breaks this may be the issue
            # updates = ""
            # pass
        num_updates = len(updates.splitlines()) - self.subtr
        return self.format_num_updates(num_updates)

    def format_num_updates(self, num):
        """
        Formats the number of updates available the way *I* like!
        Not very customizable because if you want to customize this you can
        just change the source code.
        """
        if num == 0:
            return "  0 Pkg"
        if num < 10:
            return f"  {num} Pkg"
        if num < 100:
            return f" {num} Pkg"
        if num < 1000:
            return f"{num} Pkg"
        return "999+Pkg"

    def tick(self):
        def worker():
            try:
                self.qtile.call_soon_threadsafe(self.update, "... Pkg")
                text = self.poll()
                if self.qtile is not None:
                    self.qtile.call_soon_threadsafe(self.update, text)
            except:  # noqa: E722
                logger.exception(
                    "problem polling to update widget %s", self.name
                )

        threading.Thread(target=worker).start()

    def button_press(self, x, y, button):
        base._TextBox.button_press(self, x, y, button)
        # if button == 1 and self.execute is not None:
        #     subprocess.Popen(self.execute, shell=True)
        if button == 3:
            self.tick()
        elif button == 2:
            self.qtile.call_soon_threadsafe(self.update, "  0 Pkg")


class CPU(OldCPU):
    """
    Shows system CPU use percentage.
    """

    def poll(self):
        """
        Returns custom format for CPU use percentage.
        """
        load = round(psutil.cpu_percent(), 1)
        if load < 10:
            return f" {load:1.1f}%"
        if load < 100:
            return f"{load:2.1f}%"
        return " 100%"


class Memory(OldMemory):
    """
    Shows free system memory. Ignores swap because that's for normies.
    """

    def poll(self):
        """
        Returns the current available/free system memory by subtacting the used
        memory from the total memory because psutil is retarded.
        """
        mem = psutil.virtual_memory()
        total = mem.total // 1024 // 1024
        used = mem.used // 1024 // 1024
        free = total - used

        if free > 99999:
            return "99999+MB"

        padding = ""
        if free < 10:
            padding += " "
        if free < 100:
            padding += " "
        if free < 1000:
            padding += " "
        if free < 10000:
            padding += " "
        return f"{padding}{free} MB"


class Volume(OldVolume):
    """
    Displays default audio output volume.
    """

    def _update_drawer(self):
        if self.volume == -1:
            self.text = "Mute"
        elif self.volume < 10:
            self.text = f"  {self.volume}%"
        elif self.volume < 100:
            self.text = f" {self.volume}%"
        else:
            self.text = "100%"

    def button_press(self, x, y, button):
        """
        Callback when button is pressed.
        """
        if button == 5:
            if self.volume_down_command is not None:
                subprocess.call(self.volume_down_command, shell=True)
            else:
                subprocess.call(
                    self.create_amixer_command(
                        "-q", "sset", self.channel, "{}%-".format(self.step)
                    )
                )
            if self.volume != -1:
                self.volume = max(self.volume - self.step, 0)
                self._update_drawer()
                # self.bar.draw()
        elif button == 4:
            if self.volume_up_command is not None:
                subprocess.call(self.volume_up_command, shell=True)
            else:
                subprocess.call(
                    self.create_amixer_command(
                        "-q", "sset", self.channel, "{}%+".format(self.step)
                    )
                )
            if self.volume != -1:
                self.volume = min(self.volume + self.step, 100)
                self._update_drawer()
                # self.bar.draw()
        elif button == 2:
            if self.mute_command is not None:
                subprocess.call(self.mute_command, shell=True)
            else:
                subprocess.call(
                    self.create_amixer_command(
                        "-q", "sset", self.channel, "toggle"
                    )
                )
            if self.volume != -1:
                self.volume = -1
                self._update_drawer()
                # self.bar.draw()
        elif button == 1:
            if self.volume_app is not None:
                subprocess.Popen(self.volume_app, shell=True)

        self.draw()


class Thermal(OldThermal):
    def _format_sensors_output(self, sensors_out):
        """formats output of unix `sensors` command into a dict of
        {<sensor_name>: (<temperature>, <temperature symbol>), ...}.
        The temperature is rounded to the nearest integer.
        """
        temperature_values = {}
        logger.info(self.sensors_temp.findall(sensors_out))
        for name, temp, symbol in self.sensors_temp.findall(sensors_out):
            name = name.strip()
            temp = round(float(temp))
            temperature_values[name] = str(temp), symbol
        return temperature_values
