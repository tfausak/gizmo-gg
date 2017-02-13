export default {
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
      all: 'All Ranked',
      ranked1v1: 'Ranked 1v1',
      ranked2v2: 'Ranked 2v2',
      ranked3v3: 'Ranked 3v3',
      ranked3v3solo: 'Ranked 3v3 Solo'
    }
  },
  mapTemplates: function () {
    return {
      all: 'All',
      standard: 'Standard',
      wasteland: 'Wasteland',
      arc: 'Starbase ARC',
      tokyo: 'Neo Tokyo'
    }
  },
  platforms: function () {
    return [
      {
        icon: 'fa fa-steam',
        text: 'Steam',
        slug: 'steam',
        placeholder: 'Steam profile URL or ID'
      },
      {
        icon: 'icon-xbox',
        text: 'Xbox',
        slug: 'xbox',
        placeholder: 'Xbox gamertag'
      },
      {
        icon: 'icon-playstation',
        text: 'Playstation',
        slug: 'ps',
        placeholder: 'Playstation username'
      }
    ]
  }
}
