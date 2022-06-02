import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

import "./styles/homepage.css";

const app = Elm.Main.init({
    node: document.getElementById("root"),
    flags: {
        storedProfile: localStorage.getItem("__APP_PROFILE__"),
    },
});

// app.ports.sendProfileToStorage.subscribe((profile) => {
//     localStorage.setItem("__APP_PROFILE__", JSON.stringify(profile));
// });

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
