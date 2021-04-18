#' calculaValoresPagamento function
#' @param value
#' @return value
#' @export

#Fun??o principal, que retorna duas matrizes, uma com os so
calculaValoresPagamento = function(DadosServidores,TempoAteSaida,taxaAumentoSalarial,Tempo,TetoINSS){

  Rodadas = ncol(TempoAteSaida)
  SalarioContribuicao=SalarioContribuicaoAcimaDoTeto=SalarioContribuicao20AcimaDoTeto=array(data = 0, dim = c(Tempo, Rodadas))
  for (k in 1:Rodadas){
    ValorSalarioContribuicao=array(0,dim=c(dim(DadosServidores)[1] ,Tempo))
    for (j in 1:(dim(DadosServidores)[1])){
      for (t in 1:TempoAteSaida[j,k]) {
        ValorSalarioContribuicao[j,t]=(1+taxaAumentoSalarial)^(t-1)
      }
    }
    ValorSalarioContribuicao=ValorSalarioContribuicao*DadosServidores$Salario
    ValorSalarioContribuicaoAcimaTeto=ValorSalarioContribuicao-TetoINSS
    ValorSalarioContribuicaoAcimaTeto[ ValorSalarioContribuicaoAcimaTeto<0]=0
    ValorSalarioContribuicao20AcimaTeto=ValorSalarioContribuicao-TetoINSS*1.2
    ValorSalarioContribuicao20AcimaTeto[ ValorSalarioContribuicao20AcimaTeto<0]=0
    SalarioContribuicao[,k]=12*colSums(ValorSalarioContribuicao)
    SalarioContribuicaoAcimaDoTeto[,k]=12*colSums(ValorSalarioContribuicaoAcimaTeto)
    SalarioContribuicao20AcimaDoTeto[,k]=12*colSums(ValorSalarioContribuicao20AcimaTeto)
  }  
  
  
  return(list(SalarioContribuicao, SalarioContribuicaoAcimaDoTeto, SalarioContribuicao20AcimaDoTeto))
}






