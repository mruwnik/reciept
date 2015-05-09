#!/usr/bin/python
import subprocess
import re
import urllib
from datetime import datetime


BACKUP_DIR = 'backups/'

# get a list of all backups
backups = map(
    lambda backup: re.match('^(\w+)', backup).group(1),
    subprocess.check_output(['heroku', 'pgbackups']).split('\n')[2:-1]
)

# remove all backups apart from the last one
for backup in backups[:-1]:
    print subprocess.check_output(['heroku', 'pgbackups:destroy', backup])


# make a backup
print subprocess.check_output(['heroku', 'pgbackups:capture'])
backup_url = subprocess.check_output(['heroku', 'pgbackups:url'])

# download the new dump and save it in the backups folder
downloader = urllib.URLopener()
downloader.retrieve(backup_url, BACKUP_DIR + datetime.now().strftime('%Y-%m-%d') + ".dump")

