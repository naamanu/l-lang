function evaluateCode() {
  const codeInput = document.getElementById("code").value;
  const outputStepsArea = document.getElementById("outputSteps");
  const outputTraceArea = document.getElementById("outputTrace");
  const outputEnvArea = document.getElementById("outputEnv");

  // Clear previous outputs
  outputStepsArea.innerHTML =
    '<div class="text-gray-500 animate-pulse">Evaluating...</div>';
  outputTraceArea.textContent = "...";
  outputEnvArea.textContent = "...";

  fetch("/evaluate", {
    method: "POST",
    headers: {
      "Content-Type": "text/plain",
    },
    body: codeInput,
  })
    .then((response) => {
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return response.json();
    })
    .then((data) => {
      // Clear "Evaluating..." message
      outputStepsArea.innerHTML = "";

      // Handle Steps
      if (data.steps && data.steps.length > 0) {
        data.steps.forEach((step) => {
          const stepDiv = document.createElement("div");
          stepDiv.className =
            "mb-4 border-b border-gray-100 pb-2 last:border-0";

          if (step.output) {
            const outP = document.createElement("div");
            outP.className = "font-semibold text-gray-800 text-sm";
            outP.textContent = step.output;
            stepDiv.appendChild(outP);
          }

          if (step.ast) {
            const astP = document.createElement("div");
            astP.className = "text-xs text-gray-400 mt-1 font-mono";
            astP.textContent = `AST: ${step.ast}`;
            stepDiv.appendChild(astP);
          }

          outputStepsArea.appendChild(stepDiv);
        });
      } else if (!data.finalError) {
        outputStepsArea.innerHTML =
          '<div class="text-gray-400 italic">No output generated.</div>';
      }

      // Handle Final Error
      if (data.finalError) {
        const errDiv = document.createElement("div");
        errDiv.className =
          "text-red-600 font-bold bg-red-50 p-2 rounded border border-red-200 text-sm mb-4";
        errDiv.textContent = `Error: ${data.finalError}`;
        // Prepend error or append? Rust playground usually shows errors prominently.
        outputStepsArea.appendChild(errDiv);
      }

      // Handle Trace
      if (data.traceLog && data.traceLog.length > 0) {
        outputTraceArea.textContent = data.traceLog.join("\n");
      } else {
        outputTraceArea.textContent = "No trace available.";
      }

      // Handle Environment
      if (data.finalEnvironment) {
        outputEnvArea.textContent = JSON.stringify(
          data.finalEnvironment,
          null,
          2
        );
      } else {
        outputEnvArea.textContent = "No environment data.";
      }
    })
    .catch((error) => {
      outputStepsArea.innerHTML = `<div class="text-red-600 font-bold bg-red-50 p-4 rounded border border-red-200">System Error: ${error.message}</div>`;
      console.error("Error:", error);
    });
}
