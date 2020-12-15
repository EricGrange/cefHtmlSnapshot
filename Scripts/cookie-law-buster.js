(function () {
    let fn = function () {
        document.querySelectorAll(
            '#cookie-consent-style-css, #ctcc-css, #eu-cookie-law, #cookieChoiceInfo'
        ).forEach(e => e.remove());
    };
    setInterval(fn, 100);
    fn();
    document.head.insertAdjacentHTML("beforeend", "<style>*{transition: none !important}</style>");
})();
