# Project 4 Problem 1 

def Q1():
	uniq_movie = {}
	# save actor_movies.txt and actress_movies.txt in folder project4_data

	# merge actor_movies.txt and actress_movies.txt to combined.txt
	file_names = ["project4_data/actor_movies.txt", "project4_data/actress_movies.txt"]
	with open ('project4_data/combined.txt','w',encoding="ISO-8859-1") as outfile:
		for f in file_names:
			with open (f,'r',encoding="ISO-8859-1") as infile:
				for line in infile:
					# remove actor/actress who has acted in less than 10 movies
					tokens = line.strip('\n').strip('\t\t').split("\t\t")
					tmp_tokens = []
					for mov in tokens[1:]:
						sp = mov.strip('  ').split('  ')[0]
						uniq_movie[sp] = True
						tmp_tokens.append(sp)
					#print(tokens)
					# only keep the actor/actress in combined file 
					# if it has at least 11 tokens (including actor/actress itself)
					if len(tmp_tokens) >= 10:
						outfile.write(tokens[0]+'\t\t'+'\t\t'.join(tmp_tokens)+'\n')
	print('Number of unique movies:',len(uniq_movie))

	f_combined = open ('project4_data/combined.txt','r',encoding="ISO-8859-1")
	print('Number of actors and actresses:',len(f_combined.readlines()))

	# TODO: clean the merged text file to avoid double counting same movies 


if __name__ == '__main__':
	Q1()