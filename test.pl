hello:-
  List = [[5,6], [6,5], [5,4], [4,5]],
	A = 0,
	forall(
		between(0,3,I),
			(
			K is I+1,
			forall(
					between(K,3,J),
					(
						nth0(I, List, FirstElementOfI),
						nth0(0, FirstElementOfI, Hello),
						write(Hello)
					)
				)
			)
		).