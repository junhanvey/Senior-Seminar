refStats <- tibble(
  crew = games[,1],
  games = games[,2],
  penalties = tapply(df$pCount, df$crew, sum),
  hPen = tapply(df$pCount, list(df$crew, df$home), sum)[,1],
  aPen = tapply(df$pCount, list(df$crew, df$home), sum)[,2],
  hYards = tapply(df$pYards, list(df$crew, df$home), sum)[,1],
  aYards = tapply(df$pYards, list(df$crew, df$home), sum)[,2],
  wPen = tapply(df$pCount, list (df$crew, df$won), sum)[,1],
  lPen = tapply(df$pCount, list (df$crew, df$won), sum)[,2],
  wYards = tapply(df$pYards, list(df$crew, df$won), sum)[,1],
  lYards = tapply(df$pYards, list(df$crew, df$won), sum)[,2]
)