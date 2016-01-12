let package_name = "ppx_bin_prot"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_bin_prot", None)
    ],
    [ ("META", None)
    ])
  ; ("libexec",
    [ ("built_exec_ppx", Some "ppx")
    ],
    [])
  ]
