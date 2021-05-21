import "../styles/styles.css"
import { mkApp } from "../output/Pages.App/index.js"
import dynamic from "next/dynamic"
import "@fontsource/inter/variable.css"
import "../fonts/sentient/sentient.css"
import "../fonts/clash-grotesk/clash-grotesk.css"
const MonacoEditor = dynamic(import("react-monaco-editor"), { ssr: false })

export default mkApp(MonacoEditor)
