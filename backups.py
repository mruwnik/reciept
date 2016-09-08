#!/usr/bin/python
import subprocess
import re
import urllib
from datetime import datetime


BACKUP_DIR = 'backups/'

def get_backups():
    """Get a list of all backups."""
    backups = sorted(re.findall(
        '\n(\w+)\s+\d{4}-\d{2}-\d{2}',
        subprocess.check_output(['heroku', 'pg:backups', '--app', 'desolate-anchorage-3504'])
    ))
    print 'found:', ', '.join(backups)
    return backups


def download_backup(backup):
    """Download the given backup."""
    backup_url = subprocess.check_output(
        ['heroku', 'pg:backups', '--app', 'desolate-anchorage-3504', 'public-url', backup]
    )

    print 'downloading', backup_url
    # download the new dump and save it in the backups folder
    downloader = urllib.URLopener()
    downloader.retrieve(backup_url, BACKUP_DIR + datetime.now().strftime('%Y-%m-%d') + '-' + backup + ".dump")


# remove all backups apart from the last one
for backup in get_backups()[:-1]:
    print subprocess.check_output([
        'heroku', 'pg:backups', '--app', 'desolate-anchorage-3504', 'delete', backup, '--confirm', 'desolate-anchorage-3504'
    ])


# make a backup
print "making backup"
print subprocess.check_output(['heroku', 'pg:backups', '--app', 'desolate-anchorage-3504', 'capture'])

for backup in get_backups():
    download_backup(backup)
