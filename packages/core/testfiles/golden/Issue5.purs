module Issue.Five where

getAdsInDateRangeWithKeywordPerParty ∷
  GraphQLClient → Record DateRangeAndQueryInput → Aff AggregatedAdsPerParty
getAdsInDateRangeWithKeywordPerParty client input = ado
  result ← client (Gql ∷ Gql AggSumPerParty) input
  in 
    { afd: { all: result.afd_all.agg.sum, withKeyword: result.afd.agg.sum }
    , cdu: { all: result.cdu_all.agg.sum, withKeyword: result.cdu.agg.sum }
    , csu: { all: result.csu_all.agg.sum, withKeyword: result.csu.agg.sum }
    , fdp: { all: result.fdp_all.agg.sum, withKeyword: result.fdp.agg.sum }
    , gruene: { all: result.gruene_all.agg.sum, withKeyword: result.gruene.agg.sum }
    , linke: { all: result.linke_all.agg.sum, withKeyword: result.linke.agg.sum }
    , spd: { all: result.spd_all.agg.sum, withKeyword: result.spd.agg.sum }
    }
