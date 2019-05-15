if (platform.os.family == 'Android' ||
    platform.os.family == 'iOS' ||
    platform.os.family.indexOf('Phone') >= 0) {
    alert('WARNING: This is an online game. Using it over a \n' +
          'metered connection (like most mobile/cellular services) \n' +
          'may use up A LOT of data. Please make sure that you are \n' +
          'connected to a WiFi or wired network when playing if \n' +
          'your carrier charges extra for data usage.');
}
