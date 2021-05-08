function initializeApp (instance) {
    document.getElementById("loading-indicator").style.display = "none";

    let inpEl = document.getElementById("input");
    let outEl = document.getElementById("output");

    let submitEl = document.getElementById("submit");
    let copyToClipboardEl = document.getElementById("copy-clipboard");

    let convertSuccessIndicator = document.getElementById("convert-success-indicator");
    let copySuccessIndicator = document.getElementById("copy-success-indicator");

    submit.onclick = async () => {
        if (submit.disabled) {
            console.log("Submit prevented because button is disabled.")
            return;
        }

        let inp = inpEl.value;

        convertSuccessIndicator.innerText = "";

        submitEl.innerText = "Converting...";
        submitEl.disabled = true;

        let out = await instance.exports.tryStrToPronounciation(inp);
        if (out.type === "success") {
            outEl.value = out.data;
            outEl.disabled = false;

            copyToClipboardEl.disabled = false;

            convertSuccessIndicator.style.color = "green";
            convertSuccessIndicator.style.fontWeight = "bold";
            convertSuccessIndicator.innerHTML = "&nbsp;Success!";
        } else {
            outEl.value = "ERROR: " + out.data;
            outEl.disabled = false;

            copyToClipboardEl.disabled = true;

            convertSuccessIndicator.style.color = "#9F000F";
            convertSuccessIndicator.style.fontWeight = "bold";
            convertSuccessIndicator.innerHTML = "&nbsp;Error.";
        }

        submitEl.innerText = "Convert";
        submitEl.disabled = false;
    }

    copyToClipboardEl.onclick = async () => {
        if (copyToClipboardEl.disabled) {
            console.log("Submit prevented because button is disabled.")
            return;
        }

        try {
            copyToClipboardEl.disabled = true;
            await navigator.clipboard.writeText(outEl.value);

            copySuccessIndicator.style.color = "green";
            copySuccessIndicator.style.fontWeight = "bold";
            copySuccessIndicator.innerHTML = "&nbsp;Success!";
        } catch (e) {
            copySuccessIndicator.style.color = "#9F000F";
            copySuccessIndicator.style.fontWeight = "bold";
            copySuccessIndicator.innerHTML = "&nbsp;Error.";
        }

        copyToClipboardEl.disabled = false;
    }
}
