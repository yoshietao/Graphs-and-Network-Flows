import numpy as np
from collections import OrderedDict
from collections import defaultdict
# Project 4 Problem 1 

def Q1():
	uniq_act = OrderedDict()
	act_movie = defaultdict(list)
	Num_actor_actress = 0
	uniq_movie = {}
	# save actor_movies.txt and actress_movies.txt in folder project4_data

	# merge actor_movies.txt and actress_movies.txt to combined.txt
	file_names = ["project4_data/actor_movies.txt", "project4_data/actress_movies.txt"]
	outfile =  open('project4_data/combined.txt','w',encoding="ISO-8859-1")
	for f in file_names:
		with open (f,'r',encoding="ISO-8859-1") as infile:
			for line in infile:
				# remove actor/actress who has acted in less than 10 movies
				# Preprocess: delete extra infos that makes the same movie different
				# Clean the merged text file to avoid double counting same movies 
				tokens = line.strip('\n').strip('\t\t').split("\t\t")
				# if it has at least 11 tokens (including actor/actress itself)
				if len(tokens)>=11:
					tmp_tokens = []
					for mov in tokens[1:]:
						sp = mov.strip('  ').split('  ')[0]
						uniq_movie[sp] = True
						tmp_tokens.append(sp)
					#detect duplicate actors and actresses
					if tokens[0] not in uniq_act:
						uniq_act[tokens[0]] = Num_actor_actress
						Num_actor_actress += 1
						act_movie[tokens[0]] = tmp_tokens
					else:
						act_movie[tokens[0]].extend(tmp_tokens)
	
	Num_unique_movies = len(uniq_movie)
	print('Number of unique movies:', Num_unique_movies)
	print('Number of actors and actresses:', Num_actor_actress)

	#create adjacency matrix
	adjacency_matrix = np.zeros([Num_actor_actress,Num_actor_actress])
	for act1,id1 in uniq_act.items():
		for act2,id2 in uniq_act.items():
			intersect = [val for val in act_movie[act1] if val in act_movie[act2]]
			adjacency_matrix[id1,id2] = len(intersect)/len(act_movie[act1])
			print(adjacency_matrix[id1,id2])

	#Output adjacency matrix:
	print('Output adjacency matrix...')
	for i in Num_actor_actress:
		for j in Num_actor_actress:
			outfile.write(adjacency_matrix[i,j]+' ')
		outfile.write(' \n')

if __name__ == '__main__':
	Q1()






