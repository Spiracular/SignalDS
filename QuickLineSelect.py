#Quick Python Script for AWS
import random

samples = random.sample(range(1,231780), 23178)

f=open('reviews_Video_Games_5.json', 'r')
nf = open('subset_reviews.json', 'w')

for i, line in enumerate(f):
	if (i-1) in samples:
		nf.write(line)
f.close()
nf.close()

#...yields twice as many lines as expected, which seems concerning?
#might just be that I'm using too many newlines, though.
#Yep! That.



f = open('subset_reviews.json', 'r')
l = f.readline()
f.close()
nf = open('vidreviews.txt', 'w')
import json
d = json.loads(l)
print(d)
print(d['reviewText'])



d = json.loads(l)




import json
f = open('subset_reviews.json', 'r')
#nf = open('vidreviews.txt', 'w')
with open('vidreviews.txt', 'w') as nf:
	for x,line in enumerate(f):
			d = json.loads(line)
			nf.write(d['reviewText'])
			nf.flush()
f.close()


#AUGH NONE OF THIS WORKED!

#THIS ONE NOW WORKS.

mport json
f = open('subset_reviews.json', 'r')
nf = open('vidreviews.txt', 'w')
for x,line in enumerate(f):
        if x<100:
                d = json.loads(line)
                nf.write(str(d["reviewText"])+"\n")
                #nf.write(str(d['reviewText']))
                #print(str( d['reviewText']))
f.close()
nf.close()

#ctrl-d leaves an EC2 instance

