export default {
  shortTimes: function () {
    return {
      season: 'All',
      month: '1M',
      week: '1W',
      day: '1D'
    }
  },
  times: function () {
    return {
      season: 'Current Season',
      month: 'Last Month',
      week: 'Last Week'
    }
  },
  tiers: function () {
    return {
      all: 'All',
      prospect: 'Prospect',
      challenger: 'Challenger',
      star: 'Star',
      champion: 'Champion'
    }
  },
  playlists: function () {
    return {
      all: 'All',
      ranked1v1: '1v1',
      ranked2v2: '2v2',
      ranked3v3: '3v3',
      ranked3v3solo: '3v3 Solo'
    }
  },
  mapTemplates: function () {
    return {
      all: 'All',
      standard: 'Standard',
      wasteland: 'Wasteland',
      arc: 'Starbase ARC'
    }
  }
}
