import Language.Dockerfile

main = putStr $ toDockerfileStr $ do
    from "ubuntu"
    run "apt-get update -y"