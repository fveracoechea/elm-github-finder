import { Elm } from "./Main.elm";

import "../styles/homepage.css";
import "../styles/header.css";

const app = Elm.Main.init({
    node: document.getElementById("root"),
    flags: {
        storedProfile: localStorage.getItem("__APP_PROFILE__"),
    },
});

// app.ports.sendProfileToStorage.subscribe((profile) => {
//     localStorage.setItem("__APP_PROFILE__", JSON.stringify(profile));
// });
