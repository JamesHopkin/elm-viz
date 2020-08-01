import os	
from PIL import Image

ROOT = 'dist/images'
ATLAS_SIZE = 128, 128
ATLAS_FILENAME = 'test.png'


def charIndex(c): return c - ord('a') if c.islower() else c - ord('A')

words = ['Push', 'You', 'Link', 'is']

for word in words:
	for c in word:
		pass


# pasting test

source = Image.open(os.path.join(ROOT, 'blah.png'))
target = Image.new('RGBA', ATLAS_SIZE)

example_clip = source.crop((24*8, 0, 24*9, 32))
target.paste(example_clip, (24, 32, 48, 64))
target.paste(example_clip, (48, 32, 72, 64))

with open(os.path.join(ROOT, ATLAS_FILENAME), 'wb') as out_file:
	target.save(out_file)

