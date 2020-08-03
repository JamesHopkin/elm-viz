import os	
import json
import random
import PIL
from PIL import Image


PATH = 'scripts/words.json'
PATH_OUT = 'scripts/code.txt'
ROOT = 'dist/images'
ATLAS_SIZE = 256, 256
ATLAS_FILENAME = 'test.png'

with open(PATH, "r") as read_file:
	data = json.load(read_file)

source = Image.open(os.path.join(ROOT, 'alphabet.png'))
target = Image.new('RGBA', ATLAS_SIZE)

##returns alphabet index of letter
def charIndex(c): return ord(c) - ord('a') if c.islower() else ord(c) - ord('A')

##array of character pixel widths
uc_array = [6,6,6,6,6,6,6,6,3,6,6,6,7,6,6,6,6,6,6,7,6,7,7,7,7,6]
lc_array = [6,6,6,6,6,6,6,6,3,5,6,5,7,6,6,6,6,5,6,6,6,7,7,7,7,6]

words = []
for arr in data:
	words.append(arr[1])

data_out = []

x=0
y=0
for word in words:
	##this loop is the word wrap
	word_w = 0
	for c in word: 
		if c.islower():
			word_w += lc_array[charIndex(c)]
		else:
			word_w +=  uc_array[charIndex(c)]
	if x+word_w > 256:
			y+=32
			x=0

	## this loop actually does everything
	for c in word:
		letter_start = charIndex(c)*16
		if c.islower():
			letter_end = letter_start + lc_array[charIndex(c)]
			letter_base=16 
		else:
			letter_end = letter_start + uc_array[charIndex(c)]
			letter_base=0
		c_clip = source.crop((letter_start, letter_base, letter_end, letter_base+16))

		target.paste(c_clip, (x, y))
		w = letter_end-letter_start
		x+=w #letter width without c.islower()
		
	data_out.append([x-word_w,y,word_w,16]) #x,y,w,h
	x+=8 #space between words

##testing it works
# r = random.randint(0,35)
# test = data_out[r]
# clip = (Image.open(os.path.join(ROOT, 'test2.png'))).crop((test[0], test[1], test[0]+test[2], test[1] + test[3]))
# print(test)
# print(data[r])
# target.paste(clip, (0, 0))

#'''( 'A', {text = PredicateText All, word = 'All', glyph = {x = 0, y = 0, width = 7, height = 16}} )'''
#FROM
#[ "^", "all", "PredicateText All" ],
data_out_out = []
for (i,j) in zip(data, data_out):
	x = "    ( '" + i[0] + "', " 
	if i[2]:
		x += "{text = "+ i[2] + ', word = "' + i[1] 
	else:
		x += "{text = "+ "NounText (Noun '" + i[0] + "')" + ', word ="' + i[1] 
	x += '", glyph = { x = ' + str(j[0]) + ", y = " + str(j[1]) + ", width = " + str(j[2]) + ", height = " + str(j[3]) + "}} "
	x += ")"
	data_out_out.append(x)

with open(PATH_OUT, "w") as write_file:
	write_file.write((",\n").join(data_out_out))

with open(os.path.join(ROOT, ATLAS_FILENAME), 'wb') as out_file:
	target.save(out_file)

