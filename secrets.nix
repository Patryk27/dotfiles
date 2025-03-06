let
  fw = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6iyJ3p4oMHPTgkrD9t3vHtylz7jBLFsT+8WKBAhjbDDL5nt7CM08OK6oB0TohMVvHjc0lIeuV0kKXqsZzzZZYfsgukJkmT/DhYVImBd2jHh5146kGCI65hAalT5E7TbaQ9Bg18kXonNTJidlNEZRMpbciNA4ef1f1syfuTt/ZcoGKdOzsGcKjsBdewIj5VUiXpcYGePc5zz0gWuj9hcHDFV9H2SphJuD6hh75LTGYioxidDqRqMvTfOjpY+mkElcnv+MqLGsfoTSvEPZpOFS1l+kUZ92kBRjgjDbfkaXJb+ZuRKc4f09IjOQIKFSXUPGNJqSOcrVQAfAGxUokCH+/XzSL7at8lIMI8uH1HJMuK/W584oLpqDkuEklOMDoiYMf5D4392KQKuL8YXOntMPtOyLVlS86bfINPjhD0uylNMY/8zjjJ3AWUgoI4Su6GJG4BRdBuHNQDjoMrDes9zQRuVIgcxhaoO3teggvEVYxem3UvNpE9M+eYjx/JdpJWik=";

in
{
  "secrets/fw-backup".publicKeys = [ fw ];
  "secrets/gateway-backup".publicKeys = [ fw ];
  "secrets/warp-backup".publicKeys = [ fw ];
}
