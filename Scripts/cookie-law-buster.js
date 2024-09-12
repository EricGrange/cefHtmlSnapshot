(function () {
    let fn = function () {
        document.querySelectorAll(
            '#cookie-consent-style-css, #ctcc-css, #eu-cookie-law, #cookieChoiceInfo, .fc-consent-root, #cookiescript_injected_wrapper'
        ).forEach(e => e.remove());
    };
    setInterval(fn, 100);
    fn();
	document.body.insertAdjacentHTML(
		"afterend",
		"<style>*{transition: none !important} div.fc-consent-root {display: none !important}</style>"
	);
})();
