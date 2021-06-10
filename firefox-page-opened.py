"""python firefox-page-opened.py <string>

If a page in Firefox is currently open and visiting a URL containing <string>,
exit without error.
"""


import sys
import pathlib
import json
import configparser


try:
    import lz4.block
except ModuleNotFoundError:
    print("LZ4 module not installed. Just opening a new tab regardless.")
    sys.exit(1)


def find_firefox_current_session():
    "Return the default session JSONLZ4 file."
    moz = pathlib.Path("~/.mozilla/firefox/").expanduser()
    profiles_ini = configparser.ConfigParser()
    profiles_ini.read(moz.joinpath("profiles.ini"))
    profile = profiles_ini["Profile0"]["path"]
    return moz.joinpath(profile, "sessionstore-backups", "recovery.jsonlz4")


# "rb" is important. We need to read it first as binary.
with open(
    find_firefox_current_session(),
    "rb",
) as f:
    f.seek(8)  # Seek through Mozilla's 8 byte magic header
    data = json.loads(lz4.block.decompress(f.read()))


def first_in_current_pages(func):
    """Find the first visiting page that satisfies `func`.

    Only searches through currently open pages, not histories of each tab."""
    for window in data["windows"]:
        for tab in window["tabs"]:
            try:
                # The index is 1-based. Convert to 0-based.
                entry = tab["entries"][tab["index"] - 1]
                if func(entry):
                    return entry
            # Just in case indexes break
            except IndexError:
                print(
                    "Entry length: {0}, index: {1}".format(
                        len(tab["entries"]), tab["index"]
                    )
                )
                continue
    return None


if first_in_current_pages((lambda entry: (sys.argv[1] in entry["url"]))):
    sys.exit(0)
else:
    sys.exit(1)
