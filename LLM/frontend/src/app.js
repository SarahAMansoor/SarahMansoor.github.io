import React from 'react';
import QueryForm from './components/QueryForm';
import './styles/main.css';

function App() {
    return (
        <div className="App">
            <header className="App-header">
                <h1>LLM Interaction</h1>
                <p>Welcome to the LLM frontend application. Please enter your query below:</p>
            </header>
            <main>
                <QueryForm />
            </main>
        </div>
    );
}

export default App;