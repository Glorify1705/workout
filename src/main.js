import * as React from "react";
import * as ReactDomClient from "react-dom/client";
import * as App$RescriptReactRealworldExampleApp from "./App.bs.js";
import styles from './styles.css';

const root = ReactDomClient.createRoot(document.querySelector("#root"));
const app = App$RescriptReactRealworldExampleApp.WorkoutTracker.make;

root.render(React.createElement(React.StrictMode, {
    children: React.createElement(app, {})
}));
