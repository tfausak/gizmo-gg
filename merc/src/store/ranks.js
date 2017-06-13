var _ = require('lodash')

export default {
  data: {
    'bronze': {
      name: 'Bronze',
      color: '#8f5902',
      tiers: {
        1: { name: 'B1' },
        2: { name: 'B2' },
        3: { name: 'B3' }
      }
    },
    'silver': {
      name: 'Silver',
      color: '#babdb6',
      tiers: {
        4: { name: 'S1' },
        5: { name: 'S2' },
        6: { name: 'S3' }
      }
    },
    'gold': {
      name: 'Gold',
      color: '#c4a000',
      tiers: {
        7: { name: 'G1' },
        8: { name: 'G2' },
        9: { name: 'G3' }
      }
    },
    'platinum': {
      name: 'Platinum',
      color: '#729fcf',
      tiers: {
        10: { name: 'P1' },
        11: { name: 'P2' },
        12: { name: 'P3' }
      }
    },
    'diamond': {
      name: 'Diamond',
      color: '#204A87',
      tiers: {
        13: { name: 'D1' },
        14: { name: 'D2' },
        15: { name: 'D3' }
      }
    },
    'champion': {
      name: 'Champion',
      color: '#5c3566',
      tiers: {
        16: { name: 'C1' },
        17: { name: 'C2' },
        18: { name: 'C3' }
      }
    },
    'grand-champion': {
      name: 'Grand Champion',
      color: '#ff00ff',
      tiers: {
        19: { name: 'GC' }
      }
    }
  },
  getRankObjFromTier: function (tier) {
    let found = null
    _.each(this.data, function (rank, key) {
      if (_.has(rank.tiers, tier)) {
        found = rank
      }
    })
    return found
  }
}
