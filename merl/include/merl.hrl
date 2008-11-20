-record(webapp,
	{name,
	 routes=[]}).

-record(route,
	{url="",
	 params=[],
	 target,
	 method=[get, post]}).
