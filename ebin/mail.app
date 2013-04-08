{application,mail,
[{description,"IRC Lite"},
{vsn,"1.0"},
{module,[mail_app,server,client,write_pt,read_pt,
         connector]},
{registered,[server]},
{applications,[kernel,stdlib]},
{mod,{mail_app,[]}},
{start_phases,[]}
]}.
