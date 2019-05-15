(function () {
    var xhr = new XMLHttpRequest;
    xhr.open('GET', 'https://www.tootsbook.com/tootsbook/feed?format=rss');
    xhr.onload = function (feed) {
        var latest = feed.getElementsByTagName('item')[0];
        document.querySelector('#tootsbook-feed').innerHTML = '<small>Latest News on Tootsbook: &nbsp; </small>' +
            '<a href="' + latest.querySelector('link').innerText + '">' +
            latest.querySelector('title').innerText + '</a>';
    };
    xhr.send();
})();
