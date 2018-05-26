import numpy as np
from collections import OrderedDict
from collections import defaultdict
# Project 4 Problem 1 

def Q1():
	uniq_act = OrderedDict()
	act_movie = defaultdict(list)
	Num_actor_actress = 0
	Num_unique_movies = 0
	uniq_movie = OrderedDict()
	# save actor_movies.txt and actress_movies.txt in folder project4_data

	# merge actor_movies.txt and actress_movies.txt to combined.txt
	file_names = ["project4_data/actor_movies.txt", "project4_data/actress_movies.txt"]
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
						if sp not in uniq_movie:
							uniq_movie[sp] = Num_unique_movies
							Num_unique_movies += 1
						tmp_tokens.append(sp)
					#detect duplicate actors and actresses
					if tokens[0] not in uniq_act:
						uniq_act[tokens[0]] = Num_actor_actress
						Num_actor_actress += 1
						act_movie[tokens[0]] = tmp_tokens
					else:
						act_movie[tokens[0]].extend(tmp_tokens)
	
	print('Number of unique movies:', Num_unique_movies)
	print('Number of actors and actresses:', Num_actor_actress)

	#rev_uniq_movie = {v: k for k, v in uniq_movie.items()}

	
	act_movie2 = np.zeros([Num_actor_actress,Num_unique_movies])
	for act,i in uniq_act.items():
		for m in act_movie[act]:
			act_movie2[i,uniq_movie[m]] = 1
	
	outfile =  open('project4_data/edge_list1.txt','w',encoding="ISO-8859-1")

	print('create edge list...')
	for i in range(Num_unique_movies):
		x = act_movie2[:,i]
		index = np.where(x==1)[0]

		for j in index:
			for k in index:
				if j != k:
					outfile.write(str(j)+' '+str(k)+' '+str(sum(np.multiply(act_movie2[j],act_movie2[k]))/sum(act_movie2[j]))+'\n')


if __name__ == '__main__':
	Q1()






