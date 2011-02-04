import SendFFI

main = do
  putStrLn "foo"
  initMTP
  sendMP3 "/hd/media/music/Cujo/Adventures In Foam/06 A Vida.mp3" "test.mp3"
  endMTP
