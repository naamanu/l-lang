import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import type { OnMount } from "@monaco-editor/react";
import {
  AlertCircle,
  CheckCircle,
  Copy,
  Download,
  FolderOpen,
  Info,
  Play,
  Plus,
  Save,
  Settings,
  Share,
  Trash2,
} from "lucide-react";
import { lazy, Suspense, useEffect, useRef, useState } from "react";
import { Toaster, toast } from "sonner";
import { evaluateCode as requestEvaluation, type Diagnostic, type RuntimeValue, type StepResult } from "./services/api";
import { sampleSnippets } from "./data/sample-snippets";

const Editor = lazy(() => import('./components/CodeEditor'));

export type Snippet = { title: string; code: string };


interface LintError {
  type: "error" | "warning";
  message: string;
  line?: number;
}

export default function App() {
  const [code, setCode] = useState<string>("");
  const [result, setSteps] = useState<StepResult[]>([]);
  const [env, setEnv] = useState<Record<string, RuntimeValue>>({});
  const [error, setError] = useState<string>("");
  const [snippets, setSnippets] = useState<Array<Snippet>>([]);
  const [isEvaluating, setIsEvaluating] = useState(false);
  const [errors, setErrors] = useState<Array<LintError>>([]);
  const [showSnippets, setShowSnippets] = useState(false);
  const [evalLogs, setEvalLogs] = useState<string[]>([]);
  const [diagnostic, setDiagnostic] = useState<Diagnostic | null>(null);
  const [traceTruncated, setTraceTruncated] = useState(false);
  const [editorReady, setEditorReady] = useState(false);
  const editorRef = useRef<Parameters<OnMount>[0] | null>(null);
  const monacoRef = useRef<Parameters<OnMount>[1] | null>(null);
  const activeRequest = useRef<AbortController | null>(null);

  useEffect(() => () => activeRequest.current?.abort(), []);
  useEffect(() => {
    const model = editorRef.current?.getModel();
    const monaco = monacoRef.current;
    if (!model || !monaco) return;
    monaco.editor.setModelMarkers(model, 'l-lang', diagnostic ? [{
      severity: monaco.MarkerSeverity.Error,
      message: diagnostic.message,
      startLineNumber: diagnostic.span.start.line,
      startColumn: diagnostic.span.start.column,
      endLineNumber: diagnostic.span.end.line,
      endColumn: diagnostic.span.end.column,
    }] : []);
  }, [diagnostic, editorReady]);

  useEffect(() => {
    const urlParams = new URLSearchParams(window.location.search);
    const encodedCode = urlParams.get("code");

    if (encodedCode) {
      try {
        const decodedCode = new TextDecoder("utf-8", { fatal: true }).decode(Uint8Array.from(atob(encodedCode), c => c.charCodeAt(0)));
        setCode(decodedCode);
        toast.success("Code loaded from share link!", {
          description: "Successfully loaded shared code",
          duration: 3000,
        });

        window.history.replaceState(null, "", window.location.pathname);
      } catch {
        toast.error("Failed to load shared code", {
          description: "The share link appears to be invalid",
          duration: 3000,
        });
      }
    }
  }, []);

  useEffect(() => {
    const snips = window.localStorage.getItem("snippets");
    if (!snips) {
      // Load sample snippets if no saved snippets exist
      const initialSnippets = sampleSnippets.map(sample => ({
        title: sample.title,
        code: sample.code
      }));
      setSnippets(initialSnippets);
      window.localStorage.setItem("snippets", JSON.stringify(initialSnippets));
    } else {
      try {
        const saved: unknown = JSON.parse(snips);
        if (!Array.isArray(saved) || !saved.every(item => item && typeof item.title === 'string' && typeof item.code === 'string')) throw new Error('Invalid snippets');
        setSnippets(saved);
      } catch {
        setSnippets(sampleSnippets.map(({ title, code }) => ({ title, code })));
        toast.error('Saved snippets could not be read. Showing the examples instead.');
      }
    }
  }, []);

  useEffect(() => {
    const newErrors: LintError[] = [];
    if (error) {
      newErrors.push({
        type: "error",
        message: error,
      });
    }
    setErrors(newErrors);
  }, [error]);

  const evaluateCode = async () => {
    if (!code.trim() || activeRequest.current) return;
    const controller = new AbortController();
    activeRequest.current = controller;
    setIsEvaluating(true);
    setError("");
    setDiagnostic(null);
    setSteps([]);
    setEnv({});
    setEvalLogs([]);
    setTraceTruncated(false);
    try {
      const response = await requestEvaluation(code, controller.signal);
      if (controller.signal.aborted) return;
      setEnv(response.finalEnvironment);
      setSteps(response.steps);
      setError(response.finalError ?? "");
      setDiagnostic(response.diagnostic);
      setEvalLogs(response.traceLog);
      setTraceTruncated(response.traceTruncated);
      if (response.finalError) toast.error("Evaluation stopped", { description: response.finalError });
      else toast.success("Evaluation complete");
    } catch (error) {
      if (controller.signal.aborted) return;
      const message = error instanceof Error ? error.message : "Evaluation failed. Please try again.";
      setError(message);
      toast.error("Evaluation failed", { description: message });
    } finally {
      if (activeRequest.current === controller) {
        activeRequest.current = null;
        if (!controller.signal.aborted) setIsEvaluating(false);
      }
    }
  };

  const handleSaveSnippet = (name?: string) => {
    if (!code.trim()) {
      toast.warning("No code to save", {
        description: "Please write some code before saving",
        duration: 2000,
      });
      return;
    }

    let newSnippets: Snippet[];
    const snippetName = name ? name : String(Date.now());
    const existingSnippet = snippets.find((s) => s.title === snippetName);

    if (!existingSnippet) {
      newSnippets = [...snippets, { title: snippetName, code }];
    } else {
      const oldSnippets = snippets.filter(
        (s) => s.title !== existingSnippet.title,
      );
      newSnippets = [...oldSnippets, { title: existingSnippet.title, code }];
    }

    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));
    setSnippets(newSnippets);

    toast.success("Snippet saved!", {
      description: `Saved as "${snippetName}"`,
      duration: 2000,
    });
  };

  const handleSelectSnippet = (title: string) => {
    if (isEvaluating) return;
    const foundSnippet: Snippet | undefined = snippets.find(
      (s) => s.title === title,
    );
    if (foundSnippet) {
      setCode(foundSnippet.code);
      setShowSnippets(false);
      toast.success("Snippet loaded!", {
        description: `Loaded "${title}"`,
        duration: 2000,
      });
    }
  };

  const handleAddNewSnippet = () => {
    if (isEvaluating) return;
    const newSnip: Snippet = { title: String(Date.now()), code: "" };
    const newSnippets = [...snippets, newSnip];
    setCode(newSnip.code);
    setSnippets(newSnippets);
    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));
    setShowSnippets(false);

    toast.success("New snippet created!", {
      description: "Start coding in your new snippet",
      duration: 2000,
    });
  };

  const handleDeleteSnippet = (title: string) => {
    const newSnippets = snippets.filter((s) => s.title !== title);
    setSnippets(newSnippets);
    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));

    toast.success("Snippet deleted!", {
      description: `"${title}" has been removed`,
      duration: 2000,
    });
  };

  const handleShare = async () => {
    if (!code.trim()) {
      toast.warning("No code to share", {
        description: "Please write some code before sharing",
        duration: 2000,
      });
      return;
    }

    try {
      const encoded = btoa(Array.from(new TextEncoder().encode(code), byte => String.fromCharCode(byte)).join(""));
      const url = new URL(window.location.pathname, window.location.origin);
      url.searchParams.set("code", encoded);
      await navigator.clipboard.writeText(url.toString());

      toast.success("Share link copied!", {
        description: "Share link has been copied to clipboard",
        duration: 3000,
      });
    } catch {
      toast.error("Failed to copy share link", {
        description: "Please try again",
        duration: 3000,
      });
    }
  };

  const handleCopy = async () => {
    if (!code.trim()) {
      toast.warning("No code to copy", {
        description: "Please write some code before copying",
        duration: 2000,
      });
      return;
    }

    try {
      await navigator.clipboard.writeText(code);
      toast.success("Code copied!", {
        description: "Code has been copied to clipboard",
        duration: 2000,
      });
    } catch {
      toast.error("Failed to copy code", {
        description: "Please try again",
        duration: 3000,
      });
    }
  };

  const handleDownload = () => {
    if (!code.trim()) {
      toast.warning("No code to download", {
        description: "Please write some code before downloading",
        duration: 2000,
      });
      return;
    }

    try {
      const blob = new Blob([code], { type: "text/plain" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = "program.l";
      a.click();
      URL.revokeObjectURL(url);

      toast.success("File downloaded!", {
        description: "program.l has been downloaded",
        duration: 2000,
      });
    } catch {
      toast.error("Failed to download file", {
        description: "Please try again",
        duration: 3000,
      });
    }
  };

  const formatOutput = () => {
    if (result && result.length > 0) {
      const lastStep = result[result.length - 1];
      return lastStep.output || "No output generated";
    }
    return "Run your code to see the output...";
  };

  const formatEvalSteps = () => {
    if (evalLogs && evalLogs.length > 0) {
      let output = "> Evaluation steps:\n\n";
      evalLogs.forEach((step) => {
        output = `${output} ${step}\n`;
      });
      return output + (traceTruncated ? "\nTrace truncated." : "");
    }
    return traceTruncated ? "Trace truncated." : "No evaluation steps available";
  };

  const formatEnvironment = () => {
    if (env && Object.keys(env).length > 0) {
      return JSON.stringify(env, null, 2);
    }
    return "No environment data available";
  };

  const formatCompiledOutput = () => {
    if (result && result.length > 0) {
      return result.map((step) => step.ast).join("\n\n");
    }
    return "Parsed AST will appear here...";
  };

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <Toaster position="bottom-right" richColors />

      <div className="bg-white border-b border-gray-200 px-4 py-3">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            <h1 className="text-xl font-semibold text-gray-900">
              L Language Playground
            </h1>
            <div className="flex items-center gap-2">
              <Badge
                variant={
                  isEvaluating
                    ? "secondary"
                    : errors?.some((e) => e.type === "error")
                      ? "destructive"
                      : "default"
                }
              >
                {isEvaluating
                  ? "Evaluating..."
                  : errors?.some((e) => e.type === "error")
                    ? "Error"
                    : "Ready"}
              </Badge>
              {!isEvaluating && errors && errors.length === 0 && (
                <CheckCircle className="w-4 h-4 text-green-500" />
              )}
            </div>
          </div>

          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={evaluateCode}
              disabled={isEvaluating}
            >
              <Play className="w-4 h-4 mr-1" />
              Run
            </Button>
            <div className="relative">
              <Button
                variant="outline"
                size="sm"
                onClick={() => {
                  setShowSnippets(!showSnippets);
                  if (!showSnippets) {
                    toast("Opening snippets", {
                      description: "Manage your saved code snippets",
                      duration: 1500,
                    });
                  }
                }}
              >
                <FolderOpen className="w-4 h-4 mr-1" />
                Snippets
              </Button>
              {showSnippets && (
                <div className="absolute top-full right-0 mt-1 w-64 bg-white border border-gray-200 rounded-md shadow-lg z-10">
                  <div className="p-2">
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={handleAddNewSnippet}
                      className="w-full mb-2"
                    >
                      <Plus className="w-4 h-4 mr-2" />
                      New Snippet
                    </Button>
                    {snippets.length > 0 && (
                      <div className="border-t pt-2">
                        {snippets.map((snippet) => (
                          <div
                            key={snippet.title}
                            className="flex items-center justify-between p-2 hover:bg-gray-50 rounded"
                          >
                            <button
                              type="button"
                              onClick={() => handleSelectSnippet(snippet.title)}
                              className="flex-1 cursor-pointer text-sm text-left"
                            >
                              {snippet.title}
                            </button>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => handleDeleteSnippet(snippet.title)}
                            >
                              <Trash2 className="w-3 h-3" />
                            </Button>
                          </div>
                        ))}
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleSaveSnippet(String(Date.now()))}
            >
              <Save className="w-4 h-4 mr-1" />
              Save
            </Button>
            <Button variant="outline" size="sm" onClick={handleCopy}>
              <Copy className="w-4 h-4 mr-1" />
              Copy
            </Button>
            <Button variant="outline" size="sm" onClick={handleShare}>
              <Share className="w-4 h-4 mr-1" />
              Share
            </Button>
            <Button variant="outline" size="sm" onClick={handleDownload}>
              <Download className="w-4 h-4 mr-1" />
              Download
            </Button>
          </div>
        </div>
      </div>

      {error && <div role="alert" className="bg-red-50 text-red-900 px-4 py-3 border-b border-red-200">{error}</div>}
      <div className="flex-1 flex">
        <div className="w-1/2 border-r border-gray-200 bg-white flex flex-col">
          <div className="border-b border-gray-200 px-4 py-2 bg-gray-50">
            <div className="flex items-center justify-between">
              <span className="text-sm font-medium text-gray-700">Editor</span>
              <div className="text-xs text-gray-500">
                Lines: {code.split("\n").length} | Chars: {code.length}
              </div>
            </div>
          </div>

          <div className="flex-1 pt-4">
            <Suspense fallback={<p role="status">Loading editor…</p>}>
            <Editor
              onMount={(editor, monaco) => {
                editorRef.current = editor;
                monacoRef.current = monaco;
                setEditorReady(true);
              }}
              value={code}
              onChange={(value) => setCode(value || "")}
              height="100%"
              defaultLanguage="plaintext"
              options={{
                ariaLabel: "L source code",
                readOnly: isEvaluating,
                fontSize: 14,
                fontFamily: 'Monaco, Menlo, "Ubuntu Mono", monospace',
                minimap: { enabled: false },
                scrollBeyondLastLine: false,
                automaticLayout: true,
                wordWrap: "on",
                lineNumbers: "on",
                renderLineHighlight: "none",
                selectOnLineNumbers: true,
              }}
            />
            </Suspense>
          </div>
        </div>

        <div className="w-1/2 bg-white flex flex-col">
          <Tabs defaultValue="output" className="flex-1 flex flex-col">
            <div className="border-b border-gray-200 px-4 py-2 bg-gray-50">
              <TabsList className="grid w-full grid-cols-5">
                <TabsTrigger value="output" className="text-xs">
                  <Play className="w-3 h-3 mr-1" />
                  Output
                </TabsTrigger>
                <TabsTrigger value="ast" className="text-xs">
                  <Info className="w-3 h-3 mr-1" />
                  AST
                </TabsTrigger>
                <TabsTrigger value="evalsteps" className="text-xs">
                  <CheckCircle className="w-3 h-3 mr-1" />
                  Eval Steps
                </TabsTrigger>
                <TabsTrigger value="environment" className="text-xs">
                  <Settings className="w-3 h-3 mr-1" />
                  Environment
                </TabsTrigger>
                <TabsTrigger value="issues" className="text-xs">
                  <AlertCircle className="w-3 h-3 mr-1" />
                  Issues ({errors?.length || 0})
                </TabsTrigger>
              </TabsList>
            </div>

            <div className="flex-1 overflow-hidden">
              <TabsContent value="output" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-900 text-green-400 p-4 rounded-md h-full overflow-auto font-mono text-sm">
                    <pre data-testid="output" aria-live="polite" className="whitespace-pre-wrap">{formatOutput()}</pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="ast" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-50 border rounded-md h-full overflow-auto">
                    <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                      {formatCompiledOutput()}
                    </pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="evalsteps" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-50 border rounded-md h-full overflow-auto">
                    <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                      {formatEvalSteps()}
                    </pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="environment" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-50 border rounded-md h-full overflow-auto">
                    <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                      {formatEnvironment()}
                    </pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="issues" className="h-full m-0">
                <div className="h-full p-4">
                  {!errors || errors.length === 0 ? (
                    <div className="flex items-center justify-center h-full text-gray-500">
                      <div className="text-center">
                        <CheckCircle className="w-12 h-12 mx-auto mb-2 text-green-500" />
                        <p>No issues found</p>
                      </div>
                    </div>
                  ) : (
                    <div className="space-y-2">
                      {errors.map((error, index) => (
                        <Card
                          key={`error-${error.message}-${index}`}
                          className={`border-l-4 p-0 ${
                            error.type === "error"
                              ? "border-l-red-500"
                              : "border-l-yellow-500"
                          }`}
                        >
                          <CardContent className="p-3">
                            <div className="flex items-start gap-2">
                              {error.type === "error" ? (
                                <AlertCircle className="w-4 h-4 text-red-500 mt-0.5" />
                              ) : (
                                <Info className="w-4 h-4 text-yellow-500 mt-0.5" />
                              )}
                              <div className="flex-1">
                                <p className="text-sm font-medium capitalize">
                                  {error.type}
                                </p>
                                <p className="text-sm text-gray-600">
                                  {error.message}
                                </p>
                                {error.line && (
                                  <p className="text-xs text-gray-500 mt-1">
                                    Line {error.line}
                                  </p>
                                )}
                              </div>
                            </div>
                          </CardContent>
                        </Card>
                      ))}
                    </div>
                  )}
                </div>
              </TabsContent>
            </div>
          </Tabs>
        </div>
      </div>

      <div className="bg-gray-100 border-t border-gray-200 px-4 py-2 text-xs text-gray-600">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            <span>L Programming Language Playground</span>
            <span>•</span>
            <span>{isEvaluating ? "Evaluating..." : "Ready"}</span>
          </div>
          <div className="flex items-center gap-4">
            <span>UTF-8</span>
            <span>•</span>
            <span>LF</span>
            <span>•</span>
            <span>Ln {code.split("\n").length}, Col 1</span>
          </div>
        </div>
      </div>
    </div>
  );
}
