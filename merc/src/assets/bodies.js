export default {
  list: [
    'Aftershock',
    'Backfire',
    'Batmobile',
    'Breakout',
    'Breakout Type-S',
    'Delorean',
    'Dominus',
    'Dominus GT',
    'Esper',
    'Gizmo',
    'Grog',
    'Hotshot',
    'Marauder',
    'Masamune',
    'Merc',
    'Octane',
    'Octane ZSR',
    'Paladin',
    'Proteus',
    'Ripper',
    'Roadhog',
    'Roadhog XL',
    'Scarab',
    'Sweettooth',
    'Takumi',
    'Takumi RXT',
    'Triton',
    'Venom',
    'Vulcan',
    'X-Devil',
    'X-Devil MK2',
    'Zippy'
  ],
  slug: function (body) {
    return body.toLowerCase().replace(' ', '-')
  }
}
