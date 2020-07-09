import React from 'react';
import { PageProps } from 'gatsby';

import loadable from '@loadable/component';

const Title = loadable(() => import('../components/Title'));

const Home: React.FC<PageProps> = () => {
  return (
    <main>
      <Title />
      <p>A TypeScript starter for Gatsby. Great for advanced users.</p>
      <p>Follow me on Twitter</p>
    </main>
  );
};

export default Home;
