import React, { useState } from 'react';

const QueryForm = ({ onSubmit }) => {
    const [query, setQuery] = useState('');

    const handleSubmit = (event) => {
        event.preventDefault();
        if (query.trim()) {
            onSubmit(query);
            setQuery('');
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            <input
                type="text"
                value={query}
                onChange={(e) => setQuery(e.target.value)}
                placeholder="Enter your query"
                required
            />
            <button type="submit">Submit</button>
        </form>
    );
};

export default QueryForm;