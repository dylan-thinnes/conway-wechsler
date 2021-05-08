import * as rts from "./rts.mjs";
import module from "./Asterius.wasm.mjs";
import req from "./Asterius.req.mjs";

module
    .then(m => rts.newAsteriusInstance(Object.assign(req, {module: m})))
    .then(i => {
        window.asteriusInstance = i;
        initializeApp(i);
    });
