import { loader } from '@monaco-editor/react';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';

// Monaco is the existing editor wrapper's installed peer. Bundle it locally so
// a reachable Haskell server is sufficient; editing does not depend on a CDN.
(self as typeof self & { MonacoEnvironment: { getWorker: () => Worker } }).MonacoEnvironment = {
  getWorker: () => new EditorWorker(),
};
loader.config({ monaco });
